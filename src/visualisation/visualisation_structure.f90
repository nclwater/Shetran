!> @brief Owns queued visualisation values between model accessors and HDF5.
!>
!> [[visualisation_interface_right]] asks this module to allocate a buffer node
!> for every due item and time, then fills the node with values returned by the
!> model-facing accessors. [[visualisation_hdf5]] later counts the queued nodes,
!> obtains each timestamp, copies one node into its six-dimensional write
!> buffer, and destroys the consumed node. [[visualisation_metadata]] owns the
!> opaque `first` and `latest` `C_PTR` handles passed between those two layers.
!>
!> Every node contains `s(column_or_list,row,layer,extra)`. The first three
!> dimensions retain the lower bounds supplied to [[FOR_NEW_TIME]]; the extra
!> dimension is `1:ext`. Each element of `s` then contains one of the following
!> member layouts:
!>
!> | Code | Stored kind and members | Current allocation status |
!> |:-----|:------------------------|:--------------------------|
!> | `BS` | Real north/east/south/west bank values. | Active. |
!> | `ES` | Integer north/east/south/west bank values. | Active, although no current catalogue item selects it. |
!> | `FS` | Integer north/east/south/west river-link values. | Legacy fill/read support only; no active allocator. |
!> | `GS` | Real square, four banks, then four river links. | Active. |
!> | `IS` | One integer square/member value. | Active, although no current catalogue item selects it. |
!> | `LS` | Real north/east/south/west river-link values. | Active. |
!> | `MS` | One real square/member value. | Active. |
!> | `NS` | Integer square, four banks, then four river links. | Active. |
!>
!> Values start at the exact sentinel `-1` in both numeric kinds. A skipped
!> mask/list position therefore remains `-1` in the HDF5 value array. The
!> compound member order exposed by [[GET_MBR]] and the extractors is:
!>
!> | Member index | Meaning |
!> |:------------:|:--------|
!> | 1 | Square/middle. |
!> | 2:5 | North, east, south, west banks. |
!> | 6:9 | North, east, south, west river links. |
!>
!> | Lifecycle stage | Public entry | Effect |
!> |:----------------|:-------------|:-------|
!> | Allocate | [[FOR_NEW_TIME]] | Appends a type-specific node, initializes its values, and updates both opaque handles. |
!> | Fill | [[SAVE_ITEMS_WORTH]] | Converts `latest` to the selected node type and stores one layer vector. |
!> | Describe | [[MBR_COUNT]], [[GET_MBR]] | Supplies the element-member extent and labels used by metadata. |
!> | Inspect | [[TIME_COUNT]], [[GET_HDF5_TIME]] | Traverses the queue or reads the first node's time without consuming it. |
!> | Extract | [[GET_HDF5_I]], [[GET_HDF5_R]] | Reorders one first-node payload, advances `first`, and deallocates that node. |
!>
!> `GET_HDF5_*` traverses the node in column, row, layer, member, extra, time
!> order. Its `szo` permutation maps those six logical indices into the fixed
!> metadata/HDF5 dimension slots; the current writer extracts one node at a
!> time, so the mapped time index is always one.
!>
!> @warning
!> Type strings and compound selectors are exact and case-sensitive. Except
!> for the zero returned by [[MBR_COUNT]] for an unknown type, dispatchers have
!> no default/error branch. An unsupported type can leave a result undefined or
!> silently perform no allocation, fill, or extraction. In particular, `FS`
!> cannot be created by [[FOR_NEW_TIME]]; current metadata never generates it.
!> @endwarning
!>
!> @warning
!> The `C_PTR` handles carry no run-time type or ownership information. Callers
!> must provide a non-null node of the exact type named by `typ`, coherent
!> bounds and dimensions, valid member/direction indices, and an associated
!> `latest` whenever `first` is associated. None of those preconditions or any
!> allocation status is checked.
!> @endwarning
!>
!> @warning
!> Extraction is destructive. After its final node is consumed, metadata's
!> `first` handle is set null but its separate `latest` handle still designates
!> deallocated storage. The current HDF5 buffer length is one, and the next
!> allocation sees null `first`, does not dereference the stale `latest`, and
!> immediately replaces it. Code outside that exact lifecycle must not use the
!> old `latest` value.
!> @endwarning
!>
!> @note
!> Fortran applies the bare `PRIVATE` statement below to the complete module;
!> only the explicit public list is the compiled API. The current FORD parser
!> applies default accessibility in source order and may show earlier constants
!> or types as public even though they are private to compiled clients.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | SHEGRAPH 2.0 | Created the in-memory visualisation buffer structures. |
!> | 2005-08-14 | Unknown | - | Added node destructors to deallocate payload arrays and fix a memory leak. |
!> | 2008-01-23 | Unknown | - | Retained explicit-shape generic input vectors for CVF/IVF compatibility. |
!> | 2020-09-08 | SB | - | Imported the visualisation sources and removed the external SHEGRAPH DLL. |
!> | 2026-03-29 | SvB | - | Replaced DEC address aliases and function-valued extractors with portable `ISO_C_BINDING` handles. |
!> | 2026-04-13 | SvB | - | Removed unused types, corrected integer traversal, simplified fills, and disabled `FS` allocation. |
!> | 2026-04-14 | SvB | - | Made an unknown member type return zero rather than an undefined count. |
!> @endhistory
MODULE visualisation_structure

   USE ISO_C_BINDING, ONLY: C_PTR, C_NULL_PTR, C_LOC, C_F_POINTER, C_ASSOCIATED

   USE MOD_PARAMETERS, ONLY: I_P
   USE MOD_ERROR, ONLY: errstat_alloc, errstat_dealloc, errstat_dealloc

   IMPLICIT NONE

   INTEGER, PARAMETER :: iundef = -1 !! Integer missing-value sentinel.
   INTEGER, PARAMETER :: i_not_exist = iundef !! Legacy alias for the integer sentinel.
   INTEGER, PARAMETER :: defi4(4) = (/iundef, iundef, iundef, iundef/) !! Four missing integer edge values.
   INTEGER, PARAMETER :: csz = 70 !! Public fixed length used for HDF5 item/member names.
   REAL, PARAMETER    :: zero = 0.0 !! Zero timestamp and output-array initializer.
   REAL, PARAMETER    :: half = 0.5 !! Private unused legacy half-value constant.
   REAL, PARAMETER    :: rundef = -1.0 !! Real missing-value sentinel.
   REAL, PARAMETER    :: r_not_exist = rundef !! Legacy alias for the real sentinel.
   REAL, PARAMETER    :: defr4(4) = (/r_not_exist, r_not_exist, r_not_exist, r_not_exist/) !! Four missing real edges.
   LOGICAL, PARAMETER :: t = .TRUE.  !! Short true constant used by saved one-time initialization flags.
   LOGICAL, PARAMETER :: f = .FALSE. !! Short false constant used by saved one-time initialization flags.

   INTEGER, PARAMETER :: no_types = 8 !! Private legacy count of storage families; unused in current code.

!> @brief Wraps one pointer to a live output-dimension loop index.
!>
!> [[get_hdf5]] keeps six instances and associates each `a` component with the
!> logical index selected for that result-array dimension by `szo`.
   TYPE aord
      INTEGER, POINTER :: a !! Current loop index mapped to one output dimension.
   END TYPE aord

!> @brief Holds four integer edge values in north/east/south/west order.
   TYPE integer_edges
      INTEGER :: e(4) = iundef !! Edge values in north/east/south/west order.
   END TYPE integer_edges
   TYPE(INTEGER_EDGES), PARAMETER :: default_integer_edges = INTEGER_EDGES(defi4) !! Missing integer-edge payload.

!> @brief Holds four real edge values in north/east/south/west order.
   TYPE real_edges
      REAL :: e(4) = rundef !! Edge values in north/east/south/west order.
   END TYPE real_edges
   TYPE(REAL_EDGES), PARAMETER :: default_real_edges = REAL_EDGES(defr4) !! Missing real-edge payload.

!> @brief Holds one integer square/middle value.
   TYPE integer_middle
      INTEGER :: m = rundef !! Square value; `rundef` is converted from real `-1.0` to integer `-1`.
   END TYPE integer_middle
   TYPE(INTEGER_MIDDLE), PARAMETER :: default_integer_middle = INTEGER_MIDDLE(r_not_exist) !! Missing integer middle.

!> @brief Holds one real square/middle value.
   TYPE real_middle
      REAL :: m = rundef !! Square/middle value.
   END TYPE real_middle
   TYPE(REAL_MIDDLE), PARAMETER :: default_real_middle = REAL_MIDDLE(r_not_exist) !! Missing real middle payload.

!> @brief Holds an integer square plus four bank and four river-link values.
   TYPE integer_middle_and_edges
      PRIVATE
      INTEGER :: m = iundef    !! Square/middle value.
      INTEGER :: b(4) = iundef !! Bank values in north/east/south/west order.
      INTEGER :: r(4) = iundef !! River-link values in north/east/south/west order.
   END TYPE integer_middle_and_edges
   TYPE(INTEGER_MIDDLE_AND_EDGES), PARAMETER :: default_integer_middle_and_edges = &
      INTEGER_MIDDLE_AND_EDGES(i_not_exist, defi4, defi4) !! Missing integer compound payload.

!> @brief Holds a real square plus four bank and four river-link values.
   TYPE real_middle_and_edges
      REAL :: m = rundef    !! Square/middle value.
      REAL :: b(4) = rundef !! Bank values in north/east/south/west order.
      REAL :: r(4) = rundef !! River-link values in north/east/south/west order.
   END TYPE real_middle_and_edges
   TYPE(REAL_MIDDLE_AND_EDGES), PARAMETER :: default_real_middle_and_edges = &
      REAL_MIDDLE_AND_EDGES(r_not_exist, defr4, defr4) !! Missing real compound payload.

!> @brief Time-list node for real bank-edge (`BS`) data.
   TYPE BS
      PRIVATE
      REAL :: time = zero !! Simulation time in hours.
      TYPE(REAL_EDGES), DIMENSION(:, :, :, :), POINTER :: s => NULL() !! Column/list, row, layer, extra payload.
      TYPE(BS), POINTER :: previous => NULL() !! Previous time node, or null at the head.
      TYPE(BS), POINTER :: next => NULL()     !! Next time node, or null at the tail.
   END TYPE BS

!> @brief Time-list node for integer bank-edge (`ES`) data.
   TYPE ES
      PRIVATE
      REAL :: time = zero !! Simulation time in hours.
      TYPE(INTEGER_EDGES), DIMENSION(:, :, :, :), POINTER :: s => NULL() !! Column/list, row, layer, extra payload.
      TYPE(ES), POINTER :: previous => NULL() !! Previous time node, or null at the head.
      TYPE(ES), POINTER :: next => NULL()     !! Next time node, or null at the tail.
   END TYPE ES

!> @brief Legacy time-list node for integer river-edge (`FS`) data.
   TYPE FS
      PRIVATE
      REAL :: time = zero !! Simulation time in hours.
      TYPE(INTEGER_EDGES), DIMENSION(:, :, :, :), POINTER :: s => NULL() !! Column/list, row, layer, extra payload.
      TYPE(FS), POINTER :: previous => NULL() !! Previous time node, or null at the head.
      TYPE(FS), POINTER :: next => NULL()     !! Next time node, or null at the tail.
   END TYPE FS

!> @brief Time-list node for real square/bank/river compound (`GS`) data.
   TYPE GS
      PRIVATE
      REAL :: time = zero !! Simulation time in hours.
      TYPE(REAL_MIDDLE_AND_EDGES), DIMENSION(:, :, :, :), POINTER :: s => NULL() !! Compound payload array.
      TYPE(GS), POINTER :: previous => NULL() !! Previous time node, or null at the head.
      TYPE(GS), POINTER :: next => NULL()     !! Next time node, or null at the tail.
   END TYPE GS

!> @brief Time-list node for integer square/middle (`IS`) data.
   TYPE IS
      PRIVATE
      REAL :: time = zero !! Simulation time in hours.
      TYPE(INTEGER_MIDDLE), DIMENSION(:, :, :, :), POINTER :: s => NULL() !! Column/list, row, layer, extra payload.
      TYPE(IS), POINTER :: previous => NULL() !! Previous time node, or null at the head.
      TYPE(IS), POINTER :: next => NULL()     !! Next time node, or null at the tail.
   END TYPE IS

!> @brief Time-list node for real river-edge (`LS`) data.
   TYPE LS
      PRIVATE
      REAL :: time = zero !! Simulation time in hours.
      TYPE(REAL_EDGES), DIMENSION(:, :, :, :), POINTER :: s => NULL() !! Column/list, row, layer, extra payload.
      TYPE(LS), POINTER :: previous => NULL() !! Previous time node, or null at the head.
      TYPE(LS), POINTER :: next => NULL()     !! Next time node, or null at the tail.
   END TYPE LS

!> @brief Time-list node for real square/middle (`MS`) data.
   TYPE MS
      PRIVATE
      REAL :: time = zero !! Simulation time in hours.
      TYPE(MS), POINTER :: previous => NULL() !! Previous time node, or null at the head.
      TYPE(MS), POINTER :: next => NULL()     !! Next time node, or null at the tail.
      TYPE(REAL_MIDDLE), DIMENSION(:, :, :, :), POINTER :: s => NULL() !! Column/list, row, layer, extra payload.
   END TYPE MS

!> @brief Time-list node for integer square/bank/river compound (`NS`) data.
   TYPE NS
      PRIVATE
      REAL :: time = zero !! Simulation time in hours.
      TYPE(INTEGER_MIDDLE_AND_EDGES), DIMENSION(:, :, :, :), POINTER :: s => NULL() !! Compound payload array.
      TYPE(NS), POINTER :: previous => NULL() !! Previous time node, or null at the head.
      TYPE(NS), POINTER :: next => NULL()     !! Next time node, or null at the tail.
   END TYPE NS

   INTERFACE SAVE_ITEMS_WORTH; MODULE PROCEDURE SAVE_ITEMS_WORTH_I, SAVE_ITEMS_WORTH_R; END INTERFACE

   PRIVATE
   PUBLIC :: FOR_NEW_TIME, SAVE_ITEMS_WORTH, TIME_COUNT, MBR_COUNT, GET_MBR, GET_HDF5_I, GET_HDF5_R, &
      GET_HDF5_TIME, csz

CONTAINS

!> @brief Returns the timestamp from the first queued node.
!>
!> The exact `typ` code selects the derived node type used to convert `first`.
!> The function reads only `time`; it neither advances the list nor changes the
!> opaque handle. [[visualisation_hdf5:write_mn]] calls it immediately before
!> consuming the same node through an integer or real extractor.
!>
!> @warning
!> `first` must be associated with a live node whose type matches `typ`. There
!> is no default branch: a null, stale, mismatched, or unknown handle/code gives
!> invalid access or leaves the result undefined.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Created timestamp access for buffered SHEGRAPH data. |
!> | 2026-03-29 | SvB | Replaced DEC address association with `C_F_POINTER` and corrected the integer-middle timestamp source. |
!> @endhistory
   REAL FUNCTION get_hdf5_time(typ, first) RESULT(r)

      TYPE(C_PTR), INTENT(INOUT) :: first !! Opaque handle to the first node; not modified on valid paths.
      CHARACTER(*), INTENT(IN)   :: typ   !! Exact two-character storage-family code.
      TYPE(BS), POINTER          :: pb    !! Converted real bank-edge node.
      TYPE(ES), POINTER          :: pe    !! Converted integer bank-edge node.
      TYPE(FS), POINTER          :: pf    !! Converted integer river-edge node.
      TYPE(GS), POINTER          :: pg    !! Converted real compound node.
      TYPE(IS), POINTER          :: pi    !! Converted integer middle node.
      TYPE(LS), POINTER          :: pl    !! Converted real river-edge node.
      TYPE(MS), POINTER          :: pm    !! Converted real middle node.
      TYPE(NS), POINTER          :: pn    !! Converted integer compound node.

      SELECT CASE (typ)
       CASE ('BS'); CALL C_F_POINTER(first, pb); r = pb%time
       CASE ('ES'); CALL C_F_POINTER(first, pe); r = pe%time
       CASE ('FS'); CALL C_F_POINTER(first, pf); r = pf%time
       CASE ('GS'); CALL C_F_POINTER(first, pg); r = pg%time
       CASE ('IS'); CALL C_F_POINTER(first, pi); r = pi%time
       CASE ('LS'); CALL C_F_POINTER(first, pl); r = pl%time
       CASE ('MS'); CALL C_F_POINTER(first, pm); r = pm%time
       CASE ('NS'); CALL C_F_POINTER(first, pn); r = pn%time
      END SELECT
   END FUNCTION get_hdf5_time

!> @brief Extracts and destroys one queued integer node.
!>
!> This public kind-specific wrapper passes `r` as the integer destination of
!> private [[get_hdf5]]. On return, `first` designates the following node or is
!> null, and the node that supplied `r` has been deallocated.
!>
!> @warning
!> Integer output requires `typ` to be one of `ES`, `FS`, `IS`, or `NS`; all
!> pointer, permutation, extent, and bound preconditions of [[get_hdf5]] apply.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Created integer buffer extraction in HDF5 dimension order. |
!> | 2026-03-29 | SvB | Converted the array-valued function to an explicit-output subroutine using `C_PTR`. |
!> @endhistory
   SUBROUTINE get_hdf5_i(typ, sz, szo, first, ilow, jlow, klow, r)
      INTEGER, INTENT(IN) :: ilow !! Lower column/list index of the node payload.
      INTEGER, INTENT(IN) :: jlow !! Lower row index of the node payload.
      INTEGER, INTENT(IN) :: klow !! Lower layer index of the node payload.
      TYPE(C_PTR), INTENT(INOUT) :: first !! First-node handle, advanced after destructive extraction.
      INTEGER, DIMENSION(6), INTENT(IN) :: sz !! Extents of the six-dimensional destination.
      INTEGER, DIMENSION(6), INTENT(IN) :: szo !! Logical-to-destination dimension permutation.
      INTEGER, DIMENSION(sz(1), sz(2), sz(3), sz(4), sz(5), sz(6)), INTENT(OUT) :: r !! Extracted integer values.
      CHARACTER(*), INTENT(IN) :: typ !! Exact integer storage-family code.
      CALL GET_HDF5(typ, sz, szo, first, ilow, jlow, klow, rint=r)
   END SUBROUTINE get_hdf5_i

!> @brief Extracts and destroys one queued real node.
!>
!> This public kind-specific wrapper passes `r` as the real destination of
!> private [[get_hdf5]]. On return, `first` designates the following node or is
!> null, and the node that supplied `r` has been deallocated.
!>
!> @warning
!> Real output requires `typ` to be one of `BS`, `GS`, `LS`, or `MS`; all
!> pointer, permutation, extent, and bound preconditions of [[get_hdf5]] apply.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Created real buffer extraction in HDF5 dimension order. |
!> | 2026-03-29 | SvB | Converted the array-valued function to an explicit-output subroutine using `C_PTR`. |
!> @endhistory
   SUBROUTINE get_hdf5_r(typ, sz, szo, first, ilow, jlow, klow, r)
      INTEGER, INTENT(IN) :: ilow !! Lower column/list index of the node payload.
      INTEGER, INTENT(IN) :: jlow !! Lower row index of the node payload.
      INTEGER, INTENT(IN) :: klow !! Lower layer index of the node payload.
      TYPE(C_PTR), INTENT(INOUT) :: first !! First-node handle, advanced after destructive extraction.
      INTEGER, DIMENSION(6), INTENT(IN) :: sz !! Extents of the six-dimensional destination.
      INTEGER, DIMENSION(6), INTENT(IN) :: szo !! Logical-to-destination dimension permutation.
      REAL, DIMENSION(sz(1), sz(2), sz(3), sz(4), sz(5), sz(6)), INTENT(OUT) :: r !! Extracted real values.
      CHARACTER(*), INTENT(IN) :: typ !! Exact real storage-family code.
      CALL GET_HDF5(typ, sz, szo, first, ilow, jlow, klow, rreal=r)
   END SUBROUTINE get_hdf5_r

!> @brief Reorders, copies, advances past, and destroys one buffer node.
!>
!> `szo` must be a permutation of `1:6`. It maps the routine's logical traversal
!> order to destination dimensions as follows:
!>
!> | `szo` entry | Logical index | Node access |
!> |:------------|:--------------|:------------|
!> | 1 | Column/list (`dii`) | `ii=ilow+dii-1` |
!> | 2 | Row (`djj`) | `jj=jlow+djj-1` |
!> | 3 | Layer (`dkk`) | `kk=klow+dkk-1` |
!> | 4 | Element member (`cc`) | Middle, bank, or river-link member. |
!> | 5 | Extra dimension (`ee`) | Fourth payload-array subscript. |
!> | 6 | Time (`tt`) | Fixed to one for this single-node extraction. |
!>
!> A saved array of [[aord]] wrappers points each destination slot at its live
!> logical index. The matching type branch converts `first`, invokes contained
!> `main_loop`, advances `first` to `%next` or `C_NULL_PTR`, then calls the
!> matching destructor. Integer codes are `ES`, `FS`, `IS`, and `NS`; real
!> codes are `BS`, `GS`, `LS`, and `MS`.
!>
!> Exactly one of `rint` and `rreal` is required by the intended private
!> contract. The public wrappers enforce that convention. If both are present,
!> only `rint` is initialized and populated; if neither is present, the node is
!> still destroyed without producing values.
!>
!> @warning
!> `sz`, `szo`, the three source lower bounds, `typ`, destination kind, member
!> count, and node allocation bounds are trusted. A repeated/out-of-range
!> permutation can leave a dimension pointer undefined; a kind/type mismatch
!> can copy an undefined local value; invalid extents or member counts can index
!> outside the source or destination. `first` must be a live matching node.
!> @endwarning
!>
!> @warning
!> The saved dimension-wrapper array is initialized without synchronization,
!> so this routine is not thread-safe or reentrant. Its destructive ownership
!> transfer also leaves the separate metadata `latest` handle stale after the
!> tail is removed; see the module lifecycle warning.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Created dimension-permuted extraction for all visualisation storage families. |
!> | 2005-08-14 | Unknown | Routed consumed nodes through payload-aware destructors to fix a memory leak. |
!> | 2026-03-29 | SvB | Replaced integer addresses with `C_PTR`, changed wrappers to subroutines, and fixed middle time access. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE get_hdf5(typ, sz, szo, first, ilow, jlow, klow, rint, rreal)
      INTEGER, INTENT(IN) :: ilow !! Source payload's lower column/list bound.
      INTEGER, INTENT(IN) :: jlow !! Source payload's lower row bound.
      INTEGER, INTENT(IN) :: klow !! Source payload's lower layer bound.
      TYPE(C_PTR), INTENT(INOUT) :: first !! First-node handle, advanced after extraction.
      INTEGER, DIMENSION(6), INTENT(IN) :: sz !! Destination extents in destination dimension order.
      INTEGER, DIMENSION(6), INTENT(IN) :: szo !! Logical traversal to destination-dimension permutation.
      INTEGER :: szii !! Number of source columns/list positions to traverse.
      INTEGER :: szjj !! Number of source rows to traverse.
      INTEGER :: szkk !! Number of source layers to traverse.
      INTEGER :: szcc !! Number of element members to traverse.
      INTEGER :: szee !! Number of extra-dimension positions to traverse.
      INTEGER :: sztt !! Mapped time extent; retained unused because one node is extracted.
      INTEGER :: ii !! Source column/list subscript.
      INTEGER :: jj !! Source row subscript.
      INTEGER :: kk !! Source layer subscript.
      INTEGER, TARGET :: dii !! One-based column/list loop index.
      INTEGER, TARGET :: djj !! One-based row loop index.
      INTEGER, TARGET :: dkk !! One-based layer loop index.
      INTEGER, TARGET :: cc  !! One-based element-member loop index.
      INTEGER, TARGET :: ee  !! One-based extra-dimension loop index.
      INTEGER, TARGET :: tt  !! Time index, fixed to one for one extracted node.
      TYPE(AORD), DIMENSION(:), POINTER, SAVE :: d !! Saved six-slot destination-index mapping.
      INTEGER, DIMENSION(sz(1), sz(2), sz(3), sz(4), sz(5), sz(6)), INTENT(OUT), OPTIONAL :: rint !! Integer result.
      REAL, DIMENSION(sz(1), sz(2), sz(3), sz(4), sz(5), sz(6)), INTENT(OUT), OPTIONAL :: rreal !! Real result.
      CHARACTER(*), INTENT(IN) :: typ !! Exact two-character storage-family code.
      TYPE(BS), POINTER :: pb !! Converted real bank-edge node.
      TYPE(ES), POINTER :: pe !! Converted integer bank-edge node.
      TYPE(FS), POINTER :: pf !! Converted integer river-edge node.
      TYPE(GS), POINTER :: pg !! Converted real compound node.
      TYPE(IS), POINTER :: pi !! Converted integer middle node.
      TYPE(LS), POINTER :: pl !! Converted real river-edge node.
      TYPE(MS), POINTER :: pm !! Converted real middle node.
      TYPE(NS), POINTER :: pn !! Converted integer compound node.

      LOGICAL, SAVE :: initial = T !! One-time allocation guard for `d`.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:get_hdf5"

      IF (initial) THEN
         initial = F
         ALLOCATE (d(6), STAT=ios)
         call errstat_alloc(ios, "d", location)
      END IF

      szii = sz(szo(1)); d(szo(1))%a => dii
      szjj = sz(szo(2)); d(szo(2))%a => djj
      szkk = sz(szo(3)); d(szo(3))%a => dkk
      szcc = sz(szo(4)); d(szo(4))%a => cc
      szee = sz(szo(5)); d(szo(5))%a => ee
      sztt = sz(szo(6)); d(szo(6))%a => tt
      IF (PRESENT(rint)) THEN; rint = 0; ELSEIF (PRESENT(rreal)) THEN; rreal = zero; END IF
      tt = 1

      SELECT CASE (TYP)
       CASE ('BS')  !real banks
         CALL C_F_POINTER(first, pb)
         CALL MAIN_LOOP('BS')
         IF (ASSOCIATED(pb%next)) THEN; first = C_LOC(pb%next); ELSE; first = C_NULL_PTR; END IF
         CALL DEALL_PB(pb)
       CASE ('ES')  !integer banks
         CALL C_F_POINTER(first, pe)
         CALL MAIN_LOOP('ES')
         IF (ASSOCIATED(pe%next)) THEN; first = C_LOC(pe%next); ELSE; first = C_NULL_PTR; END IF
         CALL DEALL_PE(pe)
       CASE ('FS')  !integer rivers
         CALL C_F_POINTER(first, pf)
         CALL MAIN_LOOP('FS')
         IF (ASSOCIATED(pf%next)) THEN; first = C_LOC(pf%next); ELSE; first = C_NULL_PTR; END IF
         CALL DEALL_PF(pf)
       CASE ('GS')  !real middle and edges
         CALL C_F_POINTER(first, pg)
         CALL MAIN_LOOP('GS')
         IF (ASSOCIATED(pg%next)) THEN; first = C_LOC(pg%next); ELSE; first = C_NULL_PTR; END IF
         CALL DEALL_PG(pg)
       CASE ('IS')  !integer middle
         CALL C_F_POINTER(first, pi)
         CALL MAIN_LOOP('IS')
         IF (ASSOCIATED(pi%next)) THEN; first = C_LOC(pi%next); ELSE; first = C_NULL_PTR; END IF
         CALL DEALL_PI(pi)
       CASE ('LS')  !real banks
         CALL C_F_POINTER(first, pl)
         CALL MAIN_LOOP('LS')
         IF (ASSOCIATED(pl%next)) THEN; first = C_LOC(pl%next); ELSE; first = C_NULL_PTR; END IF
         CALL DEALL_PL(pl)
       CASE ('MS')  !real middle
         CALL C_F_POINTER(first, pm)
         CALL MAIN_LOOP('MS')
         IF (ASSOCIATED(pm%next)) THEN; first = C_LOC(pm%next); ELSE; first = C_NULL_PTR; END IF
         CALL DEALL_PM(pm)
       CASE ('NS')  !integer middle and edges
         CALL C_F_POINTER(first, pn)
         CALL MAIN_LOOP('NS')
         IF (ASSOCIATED(pn%next)) THEN; first = C_LOC(pn%next); ELSE; first = C_NULL_PTR; END IF
         CALL DEALL_PN(pn)
      END SELECT

   CONTAINS

!> @brief Copies the selected node payload through the active dimension map.
!>
!> This contained worker iterates all non-time logical dimensions and derives
!> native payload indices from the three lower bounds. The host-associated
!> `d(1:6)` pointers reorder those indices when assigning the optional result.
!> Integer and real dispatches deliberately mirror the eight storage families;
!> compound values are delegated to [[FNS]] or [[FGS]].
!>
!> @warning
!> `text` and the present result kind must agree. A mismatch or unknown code can
!> assign uninitialized `idum`/`rdum`. Bounds, member counts, and the host
!> dimension permutation are not checked.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Consolidated integer and real node traversal in the HDF5 extractor. |
!> | 2026-03-29 | SvB | Retained the shared worker when the public extractors became subroutines. |
!> @endhistory
      SUBROUTINE main_loop(text)
         INTEGER                  :: idum !! Current integer member value before reordered assignment.
         REAL                     :: rdum !! Current real member value before reordered assignment.
         CHARACTER(*), INTENT(IN) :: text !! Exact storage-family code for the converted host node.
         DO dii = 1, szii; ii = ilow + dii - 1
            DO djj = 1, szjj; jj = jlow + djj - 1
               DO dkk = 1, szkk; kk = klow + dkk - 1
                  DO ee = 1, szee
                     DO cc = 1, szcc
                        IF (PRESENT(rint)) THEN
                           SELECT CASE (text)
                            CASE ('ES'); idum = pe%s(ii, jj, kk, ee)%e(cc)
                            CASE ('FS'); idum = pf%s(ii, jj, kk, ee)%e(cc)
                            CASE ('IS'); idum = pi%s(ii, jj, kk, ee)%m
                            CASE ('NS'); idum = FNS()
                           END SELECT
                           rint(d(1)%a, d(2)%a, d(3)%a, d(4)%a, d(5)%a, d(6)%a) = idum
                        ELSEIF (PRESENT(rreal)) THEN
                           SELECT CASE (text)
                            CASE ('BS'); rdum = pb%s(ii, jj, kk, ee)%e(cc)
                            CASE ('GS'); rdum = FGS()
                            CASE ('LS'); rdum = pl%s(ii, jj, kk, ee)%e(cc)
                            CASE ('MS'); rdum = pm%s(ii, jj, kk, ee)%m
                           END SELECT
                           rreal(d(1)%a, d(2)%a, d(3)%a, d(4)%a, d(5)%a, d(6)%a) = rdum
                        END IF
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END SUBROUTINE main_loop

!> @brief Returns one integer compound member at the host loop position.
!>
!> `cc=1` selects the square, `2:5` select north/east/south/west banks, and
!> `6:9` select north/east/south/west river links from the current `NS` node.
!>
!> @warning
!> The valid member range is exactly `1:9`; other values can subscript outside
!> the four-entry bank or river arrays.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added integer compound-member selection for extraction. |
!> @endhistory
      PURE INTEGER FUNCTION FNS()
         IF (cc == 1) THEN
            fns = pn%s(ii, jj, kk, ee)%m
         ELSEIF (cc > 1 .AND. cc < 6) THEN
            fns = pn%s(ii, jj, kk, ee)%b(cc - 1)
         ELSE
            fns = pn%s(ii, jj, kk, ee)%r(cc - 5)
         END IF
      END FUNCTION FNS

!> @brief Returns one real compound member at the host loop position.
!>
!> `cc=1` selects the square, `2:5` select north/east/south/west banks, and
!> `6:9` select north/east/south/west river links from the current `GS` node.
!>
!> @warning
!> The valid member range is exactly `1:9`; other values can subscript outside
!> the four-entry bank or river arrays.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added real compound-member selection for extraction. |
!> @endhistory
      PURE REAL FUNCTION FGS()
         IF (cc == 1) THEN
            fgs = pg%s(ii, jj, kk, ee)%m
         ELSEIF (cc > 1 .AND. cc < 6) THEN
            fgs = pg%s(ii, jj, kk, ee)%b(cc - 1)
         ELSE
            fgs = pg%s(ii, jj, kk, ee)%r(cc - 5)
         END IF
      END FUNCTION FGS
   END SUBROUTINE get_hdf5

!> @brief Deallocates one consumed real bank-edge node and its payload.
!>
!> [[get_hdf5]] advances `first` before calling this destructor. The payload is
!> deallocated, both links on `p` are nullified, and the node itself is freed.
!>
!> @warning
!> `p` and `p%s` must both be associated. Neighboring nodes are not repaired;
!> in particular, a surviving next node retains a stale `%previous` pointer.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2005-08-14 | Unknown | Added payload deallocation to fix the visualisation-buffer memory leak. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE deall_pb(p)
      TYPE(BS), POINTER :: p !! Consumed node to destroy.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:deall_pb"

      DEALLOCATE (p%s, STAT=ios)
      CALL errstat_dealloc(ios, "p%s", location)
      NULLIFY (p%previous, p%next)
      DEALLOCATE (p, STAT=ios)
      CALL errstat_dealloc(ios, "p", location)
   END SUBROUTINE deall_pb

!> @brief Deallocates one consumed integer bank-edge node and its payload.
!>
!> [[get_hdf5]] advances `first` before this payload/link/node teardown.
!>
!> @warning
!> `p` and `p%s` must be associated. Neighboring nodes are not repaired, so a
!> surviving next node retains a stale `%previous` pointer.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2005-08-14 | Unknown | Added payload deallocation to fix the visualisation-buffer memory leak. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE deall_pe(p)
      TYPE(ES), POINTER :: p !! Consumed node to destroy.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:deall_pe"

      DEALLOCATE (p%s, STAT=ios)
      CALL errstat_dealloc(ios, "p%s", location)
      NULLIFY (p%previous, p%next)
      DEALLOCATE (p, STAT=ios)
      CALL errstat_dealloc(ios, "p", location)
   END SUBROUTINE deall_pe

!> @brief Deallocates one consumed integer river-edge node and its payload.
!>
!> This legacy `FS` destructor remains available to [[get_hdf5]] even though
!> the current allocator cannot create an `FS` node.
!>
!> @warning
!> `p` and `p%s` must be associated. Neighboring nodes are not repaired, so a
!> surviving next node retains a stale `%previous` pointer.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2005-08-14 | Unknown | Added payload deallocation to fix the visualisation-buffer memory leak. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE deall_pf(p)
      TYPE(FS), POINTER :: p !! Consumed legacy node to destroy.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:deall_pf"

      DEALLOCATE (p%s, STAT=ios)
      CALL errstat_dealloc(ios, "p%s", location)
      NULLIFY (p%previous, p%next)
      DEALLOCATE (p, STAT=ios)
      CALL errstat_dealloc(ios, "p", location)
   END SUBROUTINE deall_pf

!> @brief Deallocates one consumed real compound node and its payload.
!>
!> [[get_hdf5]] advances `first` before this payload/link/node teardown.
!>
!> @warning
!> `p` and `p%s` must be associated. Neighboring nodes are not repaired, so a
!> surviving next node retains a stale `%previous` pointer.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2005-08-14 | Unknown | Added payload deallocation to fix the visualisation-buffer memory leak. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE deall_pg(p)
      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:deall_pg"
      TYPE(GS), POINTER :: p !! Consumed node to destroy.
      DEALLOCATE (p%s, STAT=ios)
      CALL errstat_dealloc(ios, "p%s", location)
      NULLIFY (p%previous, p%next)
      DEALLOCATE (p, STAT=ios)
      CALL errstat_dealloc(ios, "p", location)
   END SUBROUTINE deall_pg

!> @brief Deallocates one consumed integer middle node and its payload.
!>
!> [[get_hdf5]] advances `first` before this payload/link/node teardown.
!>
!> @warning
!> `p` and `p%s` must be associated. Neighboring nodes are not repaired, so a
!> surviving next node retains a stale `%previous` pointer.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2005-08-14 | Unknown | Added payload deallocation to fix the visualisation-buffer memory leak. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE deall_pi(p)
      TYPE(IS), POINTER :: p !! Consumed node to destroy.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:deall_pi"

      DEALLOCATE (p%s, STAT=ios)
      CALL errstat_dealloc(ios, "p%s", location)
      NULLIFY (p%previous, p%next)
      DEALLOCATE (p, STAT=ios)
      CALL errstat_dealloc(ios, "p", location)
   END SUBROUTINE deall_pi

!> @brief Deallocates one consumed real river-edge node and its payload.
!>
!> [[get_hdf5]] advances `first` before this payload/link/node teardown.
!>
!> @warning
!> `p` and `p%s` must be associated. Neighboring nodes are not repaired, so a
!> surviving next node retains a stale `%previous` pointer.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2005-08-14 | Unknown | Added payload deallocation to fix the visualisation-buffer memory leak. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE deall_pl(p)
      TYPE(LS), POINTER :: p !! Consumed node to destroy.
      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:deall_pl"
      DEALLOCATE (p%s, STAT=ios)
      CALL errstat_dealloc(ios, "p%s", location)
      NULLIFY (p%previous, p%next)
      DEALLOCATE (p, STAT=ios)
      CALL errstat_dealloc(ios, "p", location)
   END SUBROUTINE deall_pl

!> @brief Deallocates one consumed real middle node and its payload.
!>
!> [[get_hdf5]] advances `first` before this payload/link/node teardown.
!>
!> @warning
!> `p` and `p%s` must be associated. Neighboring nodes are not repaired, so a
!> surviving next node retains a stale `%previous` pointer.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2005-08-14 | Unknown | Added payload deallocation to fix the visualisation-buffer memory leak. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE deall_pm(p)
      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:deall_pm"
      TYPE(MS), POINTER :: p !! Consumed node to destroy.
      DEALLOCATE (p%s, STAT=ios)
      CALL errstat_dealloc(ios, "p%s", location)
      NULLIFY (p%previous, p%next)
      DEALLOCATE (p, STAT=ios)
      CALL errstat_dealloc(ios, "p", location)
   END SUBROUTINE deall_pm

!> @brief Deallocates one consumed integer compound node and its payload.
!>
!> [[get_hdf5]] advances `first` before this payload/link/node teardown.
!>
!> @warning
!> `p` and `p%s` must be associated. Neighboring nodes are not repaired, so a
!> surviving next node retains a stale `%previous` pointer.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2005-08-14 | Unknown | Added payload deallocation to fix the visualisation-buffer memory leak. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE deall_pn(p)
      TYPE(NS), POINTER :: p !! Consumed node to destroy.
      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:deall_pn"
      DEALLOCATE (p%s, STAT=ios)
      CALL errstat_dealloc(ios, "p%s", location)
      NULLIFY (p%previous, p%next)
      DEALLOCATE (p, STAT=ios)
      CALL errstat_dealloc(ios, "p", location)
   END SUBROUTINE deall_pn

!> @brief Allocates the element-member labels for one storage family.
!>
!> The pointer result is newly allocated and becomes caller-owned. Labels are
!> six characters long. Edge-only families return north/east/south/west bank or
!> link labels; middle-only families return `square`; compound families return
!> `square`, four bank labels, then four link labels. [[visualisation_metadata]]
!> attaches the result to each writer-facing item.
!>
!> For an unknown code, [[MBR_COUNT]] returns zero, so the function allocates a
!> zero-sized result and performs no assignment.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added member-label generation for HDF5 metadata. |
!> | 2026-04-14 | SvB | Made unknown type codes yield a zero-sized label result through `MBR_COUNT`. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   FUNCTION get_mbr(typ) RESULT(r)
      INTEGER :: n !! Number of labels returned for `typ`.
      CHARACTER(2), INTENT(IN) :: typ !! Exact two-character storage-family code.
      CHARACTER(6), DIMENSION(:), POINTER :: r !! Newly allocated caller-owned labels.
      CHARACTER(6), PARAMETER :: sq(1) = (/'square'/) !! Square/middle label.
      CHARACTER(6), PARAMETER :: bk(4) = (/'N-bank', 'E-bank', 'S-bank', 'W-bank'/) !! Bank labels.
      CHARACTER(6), PARAMETER :: rv(4) = (/'N-link', 'E-link', 'S-link', 'W-link'/) !! River-link labels.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "visualisation_structure:get_mbr"

      n = MBR_COUNT(typ)

      ALLOCATE (r(n), STAT=ios)
      CALL errstat_alloc(ios, "r", location)

      SELECT CASE (typ)
       CASE ('BS'); r = bk
       CASE ('ES'); r = bk
       CASE ('FS'); r = rv
       CASE ('GS'); r = (/sq, bk, rv/)
       CASE ('IS'); r = sq
       CASE ('LS'); r = rv
       CASE ('MS'); r = sq
       CASE ('NS'); r = (/sq, bk, rv/)
      END SELECT
   END FUNCTION get_mbr

!> @brief Counts nodes in one nonempty visualisation time queue.
!>
!> The exact type code converts `first` and the corresponding Fortran pointer
!> walks `%next` to the tail. The opaque input handle is not changed despite its
!> `INTENT(INOUT)` declaration. [[visualisation_hdf5]] calls this only after a
!> due output has appended a node.
!>
!> @warning
!> The routine assumes a live, non-null, correctly typed `first`. For an unknown
!> code it returns the initial value one, even though no queue is traversed.
!> Cycles are not detected and would make the traversal nonterminating.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added linked-list time-count traversal for buffered writes. |
!> | 2026-03-29 | SvB | Replaced legacy integer addresses with `C_PTR` conversion. |
!> | 2026-04-13 | SvB | Corrected the `ES` and `FS` loops to test their own next pointers. |
!> @endhistory
   INTEGER FUNCTION TIME_COUNT(typ, first) RESULT(r)
      TYPE(C_PTR), INTENT(INOUT) :: first !! First-node handle; not modified on valid paths.
      CHARACTER(*), INTENT(IN) :: typ !! Exact two-character storage-family code.
      TYPE(BS), POINTER :: pb !! Real bank-edge traversal pointer.
      TYPE(ES), POINTER :: pe !! Integer bank-edge traversal pointer.
      TYPE(FS), POINTER :: pf !! Integer river-edge traversal pointer.
      TYPE(GS), POINTER :: pg !! Real compound traversal pointer.
      TYPE(IS), POINTER :: pi !! Integer middle traversal pointer.
      TYPE(LS), POINTER :: pl !! Real river-edge traversal pointer.
      TYPE(MS), POINTER :: pm !! Real middle traversal pointer.
      TYPE(NS), POINTER :: pn !! Integer compound traversal pointer.
      r = 1
      SELECT CASE (typ)
       CASE ('BS'); CALL C_F_POINTER(first, pb); DO WHILE (ASSOCIATED(pb%next)); r = r + 1; pb => pb%next; END DO
       CASE ('ES'); CALL C_F_POINTER(first, pe); DO WHILE (ASSOCIATED(pe%next)); r = r + 1; pe => pe%next; END DO
       CASE ('FS'); CALL C_F_POINTER(first, pf); DO WHILE (ASSOCIATED(pf%next)); r = r + 1; pf => pf%next; END DO
       CASE ('GS'); CALL C_F_POINTER(first, pg); DO WHILE (ASSOCIATED(pg%next)); r = r + 1; pg => pg%next; END DO
       CASE ('IS'); CALL C_F_POINTER(first, pi); DO WHILE (ASSOCIATED(pi%next)); r = r + 1; pi => pi%next; END DO
       CASE ('LS'); CALL C_F_POINTER(first, pl); DO WHILE (ASSOCIATED(pl%next)); r = r + 1; pl => pl%next; END DO
       CASE ('MS'); CALL C_F_POINTER(first, pm); DO WHILE (ASSOCIATED(pm%next)); r = r + 1; pm => pm%next; END DO
       CASE ('NS'); CALL C_F_POINTER(first, pn); DO WHILE (ASSOCIATED(pn%next)); r = r + 1; pn => pn%next; END DO
      END SELECT
   END FUNCTION TIME_COUNT

!> @brief Returns the number of element members in a storage family.
!>
!> Middle-only families contain one member, bank/river edge families contain
!> four, and compound families contain nine. An unknown or case-mismatched code
!> returns zero. [[visualisation_metadata]] uses the result for dimension four
!> and disables that dimension when the count is one.
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added storage-family member counts. |
!> | 2026-04-14 | SvB | Added the zero-valued default for unknown codes to prevent undefined results. |
!> @endhistory
   PURE INTEGER FUNCTION mbr_count(typ) RESULT(r)
      CHARACTER(*), INTENT(IN) :: typ !! Exact two-character storage-family code.
      SELECT CASE (typ)
       CASE ('BS'); r = 4
       CASE ('ES'); r = 4
       CASE ('FS'); r = 4
       CASE ('GS'); r = 9
       CASE ('IS'); r = 1
       CASE ('LS'); r = 4
       CASE ('MS'); r = 1
       CASE ('NS'); r = 9
       CASE DEFAULT; r = 0
      END SELECT
   END FUNCTION mbr_count

!> @brief Dispatches one integer layer vector into the latest item node.
!>
!> `typ` selects `ES`, `FS`, `IS`, or `NS`, converts the opaque `latest`
!> handle, and calls the matching pure store helper. The destination slice is
!> `s(a,b,klow:khigh,e)`. `d` chooses a north/east/south/west edge for edge
!> families; `c` chooses `m`, `b`, or `r` only for compound `NS` values.
!>
!> The explicit extent of `save_this` is the retained 2008 CVF/IVF-compatible
!> generic contract and requires exactly `khigh-klow+1` values.
!>
!> @warning
!> `latest` must be a live node matching `typ`; all indices and payload bounds
!> are trusted. An unsupported code silently performs no assignment, and an
!> invalid compound selector silently leaves the initialized sentinel values.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added kind-specific dispatch into integer visualisation buffers. |
!> | 2008-01-23 | Unknown | Declared `save_this` with an explicit run-time extent for CVF/IVF compatibility. |
!> | 2026-03-29 | SvB | Replaced DEC address aliases with type-specific `C_F_POINTER` conversion. |
!> | 2026-04-13 | SvB | Removed unused helper arguments from the simple integer storage families. |
!> @endhistory
   SUBROUTINE save_items_worth_i(c, typ, a, b, klow, khigh, e, d, save_this, latest)
      INTEGER, INTENT(IN) :: a !! Destination column or list-position index.
      INTEGER, INTENT(IN) :: b !! Destination row index; one for list output.
      INTEGER, INTENT(IN) :: klow !! First destination/source layer index.
      INTEGER, INTENT(IN) :: khigh !! Last destination/source layer index.
      INTEGER, INTENT(IN) :: d !! Edge direction `1:4`; ignored for middle-only storage.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      TYPE(C_PTR), INTENT(IN) :: latest !! Opaque handle to the latest matching node.
      TYPE(ES), POINTER :: ptr_e !! Converted integer bank-edge node.
      TYPE(FS), POINTER :: ptr_f !! Converted integer river-edge node.
      TYPE(IS), POINTER :: ptr_i !! Converted integer middle node.
      TYPE(NS), POINTER :: ptr_n !! Converted integer compound node.
      INTEGER, DIMENSION(khigh - klow + 1), INTENT(IN) :: save_this !! Layer vector to store.
      CHARACTER, INTENT(IN) :: c !! Compound selector `m`, `b`, or `r`; ignored by simple families.
      CHARACTER(*), INTENT(IN) :: typ !! Exact integer storage-family code.
      SELECT CASE (typ)
       CASE ('ES'); CALL C_F_POINTER(latest, ptr_e); CALL SAVE_ES(ptr_e, a, b, klow, khigh, e, d, save_this)
       CASE ('FS'); CALL C_F_POINTER(latest, ptr_f); CALL SAVE_FS(ptr_f, a, b, klow, khigh, e, d, save_this)
       CASE ('IS'); CALL C_F_POINTER(latest, ptr_i); CALL SAVE_IS(ptr_i, a, b, klow, khigh, e, save_this)
       CASE ('NS'); CALL C_F_POINTER(latest, ptr_n); CALL SAVE_NS(ptr_n, a, b, klow, khigh, e, d, save_this, c)
      END SELECT
   END SUBROUTINE save_items_worth_i

!> @brief Dispatches one real layer vector into the latest item node.
!>
!> `typ` selects `BS`, `GS`, `LS`, or `MS`, converts the opaque `latest`
!> handle, and calls the matching pure store helper. The destination slice is
!> `s(a,b,klow:khigh,e)`. `d` chooses a north/east/south/west edge for edge
!> families; `c` chooses `m`, `b`, or `r` only for compound `GS` values.
!>
!> @warning
!> `latest` must be a live node matching `typ`; all indices, extents, and bounds
!> are trusted. An unsupported code silently performs no assignment, and an
!> invalid compound selector silently retains initialized sentinel values.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added kind-specific dispatch into real visualisation buffers. |
!> | 2008-01-23 | Unknown | Retained the explicit run-time extent of `save_this` for CVF/IVF compatibility. |
!> | 2026-03-29 | SvB | Replaced DEC address aliases with type-specific `C_F_POINTER` conversion. |
!> | 2026-04-13 | SvB | Removed unused helper arguments from the simple real storage families. |
!> @endhistory
   SUBROUTINE save_items_worth_r(c, typ, a, b, klow, khigh, e, d, save_this, latest)
      INTEGER, INTENT(IN) :: a !! Destination column or list-position index.
      INTEGER, INTENT(IN) :: b !! Destination row index; one for list output.
      INTEGER, INTENT(IN) :: klow !! First destination/source layer index.
      INTEGER, INTENT(IN) :: khigh !! Last destination/source layer index.
      INTEGER, INTENT(IN) :: d !! Edge direction `1:4`; ignored for middle-only storage.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      TYPE(C_PTR), INTENT(IN) :: latest !! Opaque handle to the latest matching node.
      TYPE(BS), POINTER :: ptr_b !! Converted real bank-edge node.
      TYPE(GS), POINTER :: ptr_g !! Converted real compound node.
      TYPE(LS), POINTER :: ptr_l !! Converted real river-edge node.
      TYPE(MS), POINTER :: ptr_m !! Converted real middle node.
      REAL, DIMENSION(khigh - klow + 1), INTENT(IN) :: save_this !! Layer vector to store.
      CHARACTER, INTENT(IN) :: c !! Compound selector `m`, `b`, or `r`; ignored by simple families.
      CHARACTER(*), INTENT(IN) :: typ !! Exact real storage-family code.
      SELECT CASE (typ)
       CASE ('BS'); CALL C_F_POINTER(latest, ptr_b); CALL SAVE_BS(ptr_b, a, b, klow, khigh, e, d, save_this)
       CASE ('GS'); CALL C_F_POINTER(latest, ptr_g); CALL SAVE_GS(ptr_g, a, b, klow, khigh, e, d, save_this, c)
       CASE ('LS'); CALL C_F_POINTER(latest, ptr_l); CALL SAVE_LS(ptr_l, a, b, klow, khigh, e, d, save_this)
       CASE ('MS'); CALL C_F_POINTER(latest, ptr_m); CALL SAVE_MS(ptr_m, a, b, klow, khigh, e, save_this)
      END SELECT
   END SUBROUTINE save_items_worth_r

!> @brief Stores a real layer vector in one bank direction of a `BS` node.
!>
!> The pure assignment targets `r%s(a,b,klow:khigh,e)%e(d)`.
!>
!> @warning
!> The node payload must be associated, `d` must be in `1:4`, all other indices
!> must be within its allocation, and `SIZE(save_this)` must equal the layer
!> section length.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added real bank-edge vector storage. |
!> | 2026-04-13 | SvB | Removed the unused compound-selector argument. |
!> @endhistory
   PURE SUBROUTINE save_bs(r, a, b, klow, khigh, e, d, save_this)
      INTEGER, INTENT(IN) :: a !! Destination column/list index.
      INTEGER, INTENT(IN) :: b !! Destination row index.
      INTEGER, INTENT(IN) :: klow !! First destination layer.
      INTEGER, INTENT(IN) :: khigh !! Last destination layer.
      INTEGER, INTENT(IN) :: d !! Bank direction in north/east/south/west order.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      REAL, DIMENSION(:), INTENT(IN) :: save_this !! Layer vector to assign.
      TYPE(BS), INTENT(INOUT) :: r !! Real bank-edge node to update.
      r%s(a, b, klow:khigh, e)%e(d) = save_this
   END SUBROUTINE save_bs

!> @brief Stores an integer layer vector in one bank direction of an `ES` node.
!>
!> @warning
!> The payload must be associated; `d` must be `1:4`; all indices and the layer
!> vector extent must conform to the destination section.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added integer bank-edge vector storage. |
!> | 2026-04-13 | SvB | Removed the unused compound-selector argument. |
!> @endhistory
   PURE SUBROUTINE save_es(r, a, b, klow, khigh, e, d, save_this)
      INTEGER, INTENT(IN) :: a !! Destination column/list index.
      INTEGER, INTENT(IN) :: b !! Destination row index.
      INTEGER, INTENT(IN) :: klow !! First destination layer.
      INTEGER, INTENT(IN) :: khigh !! Last destination layer.
      INTEGER, INTENT(IN) :: d !! Bank direction in north/east/south/west order.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      INTEGER, DIMENSION(:), INTENT(IN) :: save_this !! Layer vector to assign.
      TYPE(ES), INTENT(INOUT) :: r !! Integer bank-edge node to update.
      r%s(a, b, klow:khigh, e)%e(d) = save_this
   END SUBROUTINE save_es

!> @brief Stores an integer layer vector in one river direction of an `FS` node.
!>
!> This legacy helper remains callable through integer dispatch, although the
!> current [[FOR_NEW_TIME]] cannot allocate its node type.
!>
!> @warning
!> The payload must be associated; `d` must be `1:4`; all indices and the layer
!> vector extent must conform to the destination section.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added integer river-edge vector storage. |
!> | 2026-04-13 | SvB | Removed the unused selector argument while disabling `FS` allocation. |
!> @endhistory
   PURE SUBROUTINE save_fs(r, a, b, klow, khigh, e, d, save_this)
      INTEGER, INTENT(IN) :: a !! Destination column/list index.
      INTEGER, INTENT(IN) :: b !! Destination row index.
      INTEGER, INTENT(IN) :: klow !! First destination layer.
      INTEGER, INTENT(IN) :: khigh !! Last destination layer.
      INTEGER, INTENT(IN) :: d !! River direction in north/east/south/west order.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      INTEGER, DIMENSION(:), INTENT(IN) :: save_this !! Layer vector to assign.
      TYPE(FS), INTENT(INOUT) :: r !! Integer river-edge node to update.
      r%s(a, b, klow:khigh, e)%e(d) = save_this
   END SUBROUTINE save_fs

!> @brief Stores a real layer vector in one selected `GS` compound member.
!>
!> `c='m'` assigns the square/middle member and ignores `d`; `c='b'` assigns
!> bank `d`; `c='r'` assigns river-link `d`.
!>
!> @warning
!> Only lowercase `m`, `b`, and `r` are recognized. Other selectors silently do
!> nothing. For edge members `d` must be `1:4`; all payload indices and vector
!> extents must conform.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added real square/bank/river compound vector storage. |
!> @endhistory
   PURE SUBROUTINE save_gs(r, a, b, klow, khigh, e, d, save_this, c)
      INTEGER, INTENT(IN) :: a !! Destination column/list index.
      INTEGER, INTENT(IN) :: b !! Destination row index.
      INTEGER, INTENT(IN) :: klow !! First destination layer.
      INTEGER, INTENT(IN) :: khigh !! Last destination layer.
      INTEGER, INTENT(IN) :: d !! Bank/river direction; ignored for `c='m'`.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      REAL, DIMENSION(:), INTENT(IN) :: save_this !! Layer vector to assign.
      CHARACTER, INTENT(IN) :: c !! Lowercase compound selector `m`, `b`, or `r`.
      TYPE(GS), INTENT(INOUT) :: r !! Real compound node to update.
      SELECT CASE (c)
       CASE ('m'); r%s(a, b, klow:khigh, e)%m = save_this
       CASE ('b'); r%s(a, b, klow:khigh, e)%b(d) = save_this
       CASE ('r'); r%s(a, b, klow:khigh, e)%r(d) = save_this
      END SELECT
   END SUBROUTINE save_gs

!> @brief Stores an integer layer vector in the middle member of an `IS` node.
!>
!> @warning
!> The payload must be associated, all indices must be within its allocation,
!> and the vector extent must equal `khigh-klow+1`.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added integer middle-member vector storage. |
!> | 2026-04-13 | SvB | Removed unused direction and compound-selector arguments. |
!> @endhistory
   PURE SUBROUTINE save_is(r, a, b, klow, khigh, e, save_this)
      INTEGER, INTENT(IN) :: a !! Destination column/list index.
      INTEGER, INTENT(IN) :: b !! Destination row index.
      INTEGER, INTENT(IN) :: klow !! First destination layer.
      INTEGER, INTENT(IN) :: khigh !! Last destination layer.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      INTEGER, DIMENSION(:), INTENT(IN) :: save_this !! Layer vector to assign.
      TYPE(IS), INTENT(INOUT) :: r !! Integer middle node to update.
      r%s(a, b, klow:khigh, e)%m = save_this
   END SUBROUTINE save_is

!> @brief Stores a real layer vector in one river direction of an `LS` node.
!>
!> @warning
!> The payload must be associated; `d` must be `1:4`; all indices and the layer
!> vector extent must conform to the destination section.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added real river-edge vector storage. |
!> | 2026-04-13 | SvB | Removed the unused compound-selector argument. |
!> @endhistory
   PURE SUBROUTINE save_ls(r, a, b, klow, khigh, e, d, save_this)
      INTEGER, INTENT(IN) :: a !! Destination column/list index.
      INTEGER, INTENT(IN) :: b !! Destination row index.
      INTEGER, INTENT(IN) :: klow !! First destination layer.
      INTEGER, INTENT(IN) :: khigh !! Last destination layer.
      INTEGER, INTENT(IN) :: d !! River direction in north/east/south/west order.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      REAL, DIMENSION(:), INTENT(IN) :: save_this !! Layer vector to assign.
      TYPE(LS), INTENT(INOUT) :: r !! Real river-edge node to update.
      r%s(a, b, klow:khigh, e)%e(d) = save_this
   END SUBROUTINE save_ls

!> @brief Stores a real layer vector in the middle member of an `MS` node.
!>
!> @warning
!> The payload must be associated, all indices must be within its allocation,
!> and the vector extent must equal `khigh-klow+1`.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added real middle-member vector storage. |
!> | 2026-04-13 | SvB | Removed unused direction and compound-selector arguments. |
!> @endhistory
   PURE SUBROUTINE save_ms(r, a, b, klow, khigh, e, save_this)
      INTEGER, INTENT(IN) :: a !! Destination column/list index.
      INTEGER, INTENT(IN) :: b !! Destination row index.
      INTEGER, INTENT(IN) :: klow !! First destination layer.
      INTEGER, INTENT(IN) :: khigh !! Last destination layer.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      REAL, DIMENSION(:), INTENT(IN) :: save_this !! Layer vector to assign.
      TYPE(MS), INTENT(INOUT) :: r !! Real middle node to update.
      r%s(a, b, klow:khigh, e)%m = save_this
   END SUBROUTINE save_ms

!> @brief Stores an integer layer vector in one selected `NS` compound member.
!>
!> `c='m'` assigns the square/middle member and ignores `d`; `c='b'` assigns
!> bank `d`; `c='r'` assigns river-link `d`.
!>
!> @warning
!> Only lowercase `m`, `b`, and `r` are recognized. Other selectors silently do
!> nothing. For edge members `d` must be `1:4`; all payload indices and vector
!> extents must conform.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added integer square/bank/river compound vector storage. |
!> @endhistory
   PURE SUBROUTINE save_ns(r, a, b, klow, khigh, e, d, save_this, c)
      INTEGER, INTENT(IN) :: a !! Destination column/list index.
      INTEGER, INTENT(IN) :: b !! Destination row index.
      INTEGER, INTENT(IN) :: klow !! First destination layer.
      INTEGER, INTENT(IN) :: khigh !! Last destination layer.
      INTEGER, INTENT(IN) :: d !! Bank/river direction; ignored for `c='m'`.
      INTEGER, INTENT(IN) :: e !! Extra-dimension index.
      INTEGER, DIMENSION(:), INTENT(IN) :: save_this !! Layer vector to assign.
      CHARACTER, INTENT(IN) :: c !! Lowercase compound selector `m`, `b`, or `r`.
      TYPE(NS), INTENT(INOUT) :: r !! Integer compound node to update.
      SELECT CASE (c)
       CASE ('m'); r%s(a, b, klow:khigh, e)%m = save_this
       CASE ('b'); r%s(a, b, klow:khigh, e)%b(d) = save_this
       CASE ('r'); r%s(a, b, klow:khigh, e)%r(d) = save_this
      END SELECT
   END SUBROUTINE save_ns

!> @brief Appends a default-initialized node for one item and output time.
!>
!> The exact type code dispatches to one of seven active allocators. Every
!> payload is allocated with bounds
!> `(ilow:ihigh,jlow:jhigh,klow:khigh,1:ext)` and initialized to that family's
!> integer or real `-1` payload. If `first` is null, the new node becomes the
!> head without reading the previous `latest`; otherwise it is linked after the
!> live node designated by `latest`. `latest` always becomes the new node.
!>
!> | Code | Allocator | Initial payload |
!> |:-----|:----------|:----------------|
!> | `BS` | [[FOR_NEW_TIME_BS]] | Real bank edges. |
!> | `ES` | [[FOR_NEW_TIME_ES]] | Integer bank edges. |
!> | `GS` | [[FOR_NEW_TIME_GS]] | Real square/bank/river compound. |
!> | `IS` | [[FOR_NEW_TIME_IS]] | Integer middle. |
!> | `LS` | [[FOR_NEW_TIME_LS]] | Real river edges. |
!> | `MS` | [[FOR_NEW_TIME_MS]] | Real middle. |
!> | `NS` | [[FOR_NEW_TIME_NS]] | Integer square/bank/river compound. |
!>
!> @warning
!> `FS` has no active allocation branch. Its former allocator was disabled in
!> the 2026 GFortran debug-compatibility work, although legacy fill/read support
!> remains. Any unsupported or case-mismatched code silently returns without
!> changing either handle.
!> @endwarning
!>
!> @warning
!> Bounds, positive `ext`, allocation success, and handle coherence are not
!> checked. When `first` is associated, `latest` must designate the live tail
!> node of exactly the selected type.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added type-dispatched time-node creation. |
!> | 2026-03-29 | SvB | Replaced integer addresses with `C_PTR` head/tail handles. |
!> | 2026-04-13 | SvB | Disabled the `FS` allocator during GFortran debug-compatibility work. |
!> @endhistory
   SUBROUTINE FOR_NEW_TIME(typ, time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN) :: ilow !! First column/list index.
      INTEGER, INTENT(IN) :: ihigh !! Last column/list index.
      INTEGER, INTENT(IN) :: jlow !! First row index.
      INTEGER, INTENT(IN) :: jhigh !! Last row index.
      INTEGER, INTENT(IN) :: klow !! First layer index.
      INTEGER, INTENT(IN) :: khigh !! Last layer index.
      INTEGER, INTENT(IN) :: ext !! Fourth-dimension upper bound; intended to be positive.
      TYPE(C_PTR), INTENT(INOUT) :: first !! Head handle, set only when the queue was empty.
      TYPE(C_PTR), INTENT(INOUT) :: latest !! Tail handle, replaced by the new node.
      REAL, INTENT(IN) :: time !! Simulation time in hours stored on the node.
      CHARACTER(*), INTENT(IN) :: typ !! Exact active storage-family code.
      SELECT CASE (typ)
       CASE ('BS'); CALL FOR_NEW_TIME_BS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
       CASE ('ES'); CALL FOR_NEW_TIME_ES(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
       CASE ('GS'); CALL FOR_NEW_TIME_GS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
       CASE ('IS'); CALL FOR_NEW_TIME_IS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
       CASE ('LS'); CALL FOR_NEW_TIME_LS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
       CASE ('MS'); CALL FOR_NEW_TIME_MS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
       CASE ('NS'); CALL FOR_NEW_TIME_NS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      END SELECT
   END SUBROUTINE FOR_NEW_TIME

!> @brief Allocates and appends one real bank-edge (`BS`) time node.
!>
!> The payload retains the supplied first-three-dimension bounds, uses `1:ext`
!> for its fourth dimension, and starts with all four bank members at `-1.0`.
!> A nonempty queue is linked in both directions through its current tail.
!>
!> @warning
!> Allocation/bounds are unchecked. Non-null `first` requires `latest` to be a
!> live `BS` tail; a stale or mismatched handle is invalid.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added real bank-edge time-node allocation and linking. |
!> | 2026-03-29 | SvB | Converted head/tail addresses to `C_PTR` and type-safe Fortran pointers. |
!> @endhistory
   SUBROUTINE FOR_NEW_TIME_BS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN) :: ilow !! First column/list index.
      INTEGER, INTENT(IN) :: ihigh !! Last column/list index.
      INTEGER, INTENT(IN) :: jlow !! First row index.
      INTEGER, INTENT(IN) :: jhigh !! Last row index.
      INTEGER, INTENT(IN) :: klow !! First layer index.
      INTEGER, INTENT(IN) :: khigh !! Last layer index.
      INTEGER, INTENT(IN) :: ext !! Fourth-dimension upper bound.
      TYPE(C_PTR), INTENT(INOUT) :: first !! Head handle, set when the queue is empty.
      TYPE(C_PTR), INTENT(INOUT) :: latest !! Tail handle, replaced with the new node.
      REAL, INTENT(IN) :: time !! Simulation time in hours.
      TYPE(BS), POINTER :: r !! Newly allocated node.
      TYPE(BS), POINTER :: prev_node !! Converted previous tail for a nonempty queue.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = 'FOR_NEW_TIME_BS'

      ALLOCATE (r, STAT=ios)
      CALL errstat_alloc(ios, "r", location)
      r%time = time

      ALLOCATE (r%s(ilow:ihigh, jlow:jhigh, klow:khigh, ext), STAT=ios)
      CALL errstat_alloc(ios, "r%s", location)
      r%s = default_real_edges
      IF (.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous => prev_node
         prev_node%next => r
      END IF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_BS

!> @brief Allocates and appends one integer bank-edge (`ES`) time node.
!>
!> The payload retains the supplied first-three-dimension bounds, uses `1:ext`
!> for its fourth dimension, and starts with all four bank members at `-1`.
!>
!> @warning
!> Allocation/bounds are unchecked. Non-null `first` requires `latest` to be a
!> live `ES` tail; a stale or mismatched handle is invalid.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added integer bank-edge time-node allocation and linking. |
!> | 2026-03-29 | SvB | Converted head/tail addresses to `C_PTR` and type-safe Fortran pointers. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE FOR_NEW_TIME_ES(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN) :: ilow !! First column/list index.
      INTEGER, INTENT(IN) :: ihigh !! Last column/list index.
      INTEGER, INTENT(IN) :: jlow !! First row index.
      INTEGER, INTENT(IN) :: jhigh !! Last row index.
      INTEGER, INTENT(IN) :: klow !! First layer index.
      INTEGER, INTENT(IN) :: khigh !! Last layer index.
      INTEGER, INTENT(IN) :: ext !! Fourth-dimension upper bound.
      TYPE(C_PTR), INTENT(INOUT) :: first !! Head handle, set when the queue is empty.
      TYPE(C_PTR), INTENT(INOUT) :: latest !! Tail handle, replaced with the new node.
      REAL, INTENT(IN) :: time !! Simulation time in hours.
      TYPE(ES), POINTER :: r !! Newly allocated node.
      TYPE(ES), POINTER :: prev_node !! Converted previous tail for a nonempty queue.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = 'FOR_NEW_TIME_ES'

      ALLOCATE (r, STAT=ios)
      CALL errstat_alloc(ios, "r", location)
      r%time = time
      ALLOCATE (r%s(ilow:ihigh, jlow:jhigh, klow:khigh, ext), STAT=ios)
      CALL errstat_alloc(ios, "r%s", location)
      r%s = default_integer_edges
      IF (.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous => prev_node
         prev_node%next => r
      END IF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_ES

!> @brief Allocates and appends one real compound (`GS`) time node.
!>
!> Every square, bank, and river-link member starts at `-1.0`; the payload
!> retains the supplied first-three-dimension bounds and uses `1:ext` for its
!> fourth dimension.
!>
!> @warning
!> Allocation/bounds are unchecked. Non-null `first` requires `latest` to be a
!> live `GS` tail; a stale or mismatched handle is invalid.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added real compound time-node allocation and linking. |
!> | 2026-03-29 | SvB | Converted head/tail addresses to `C_PTR` and type-safe Fortran pointers. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE FOR_NEW_TIME_GS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN) :: ilow !! First column/list index.
      INTEGER, INTENT(IN) :: ihigh !! Last column/list index.
      INTEGER, INTENT(IN) :: jlow !! First row index.
      INTEGER, INTENT(IN) :: jhigh !! Last row index.
      INTEGER, INTENT(IN) :: klow !! First layer index.
      INTEGER, INTENT(IN) :: khigh !! Last layer index.
      INTEGER, INTENT(IN) :: ext !! Fourth-dimension upper bound.
      TYPE(C_PTR), INTENT(INOUT) :: first !! Head handle, set when the queue is empty.
      TYPE(C_PTR), INTENT(INOUT) :: latest !! Tail handle, replaced with the new node.
      REAL, INTENT(IN) :: time !! Simulation time in hours.
      TYPE(GS), POINTER :: r !! Newly allocated node.
      TYPE(GS), POINTER :: prev_node !! Converted previous tail for a nonempty queue.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = 'FOR_NEW_TIME_GS'

      ALLOCATE (r, STAT=ios)
      CALL errstat_alloc(ios, "r", location)
      r%time = time

      ALLOCATE (r%s(ilow:ihigh, jlow:jhigh, klow:khigh, ext), STAT=ios)
      CALL errstat_alloc(ios, "r%s", location)
      r%s = default_real_middle_and_edges

      IF (.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL C_F_POINTER(latest, prev_node)
         r%previous => prev_node
         prev_node%next => r
      END IF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_GS

!> @brief Allocates and appends one integer middle (`IS`) time node.
!>
!> Every middle value starts at integer `-1`; the payload retains the supplied
!> first-three-dimension bounds and uses `1:ext` for its fourth dimension.
!>
!> @warning
!> Allocation/bounds are unchecked. Non-null `first` requires `latest` to be a
!> live `IS` tail; a stale or mismatched handle is invalid.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added integer middle time-node allocation and linking. |
!> | 2026-03-29 | SvB | Converted head/tail addresses to `C_PTR` and type-safe Fortran pointers. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE FOR_NEW_TIME_IS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN) :: ilow !! First column/list index.
      INTEGER, INTENT(IN) :: ihigh !! Last column/list index.
      INTEGER, INTENT(IN) :: jlow !! First row index.
      INTEGER, INTENT(IN) :: jhigh !! Last row index.
      INTEGER, INTENT(IN) :: klow !! First layer index.
      INTEGER, INTENT(IN) :: khigh !! Last layer index.
      INTEGER, INTENT(IN) :: ext !! Fourth-dimension upper bound.
      TYPE(C_PTR), INTENT(INOUT) :: first !! Head handle, set when the queue is empty.
      TYPE(C_PTR), INTENT(INOUT) :: latest !! Tail handle, replaced with the new node.
      REAL, INTENT(IN) :: time !! Simulation time in hours.
      TYPE(IS), POINTER :: r !! Newly allocated node.
      TYPE(IS), POINTER :: prev_node !! Converted previous tail for a nonempty queue.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = 'FOR_NEW_TIME_IS'

      ALLOCATE (r, STAT=ios)
      CALL errstat_alloc(ios, "r", location)
      r%time = time

      ALLOCATE (r%s(ilow:ihigh, jlow:jhigh, klow:khigh, ext), STAT=ios)
      CALL errstat_alloc(ios, "r%s", location)
      r%s = default_integer_middle

      IF (.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous => prev_node
         prev_node%next => r
      END IF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_IS

!> @brief Allocates and appends one real river-edge (`LS`) time node.
!>
!> Every river-link direction starts at `-1.0`; the payload retains the
!> supplied first-three-dimension bounds and uses `1:ext` for its fourth
!> dimension.
!>
!> @warning
!> Allocation/bounds are unchecked. Non-null `first` requires `latest` to be a
!> live `LS` tail; a stale or mismatched handle is invalid.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added real river-edge time-node allocation and linking. |
!> | 2026-03-29 | SvB | Converted head/tail addresses to `C_PTR` and type-safe Fortran pointers. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE FOR_NEW_TIME_LS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN) :: ilow !! First column/list index.
      INTEGER, INTENT(IN) :: ihigh !! Last column/list index.
      INTEGER, INTENT(IN) :: jlow !! First row index.
      INTEGER, INTENT(IN) :: jhigh !! Last row index.
      INTEGER, INTENT(IN) :: klow !! First layer index.
      INTEGER, INTENT(IN) :: khigh !! Last layer index.
      INTEGER, INTENT(IN) :: ext !! Fourth-dimension upper bound.
      TYPE(C_PTR), INTENT(INOUT) :: first !! Head handle, set when the queue is empty.
      TYPE(C_PTR), INTENT(INOUT) :: latest !! Tail handle, replaced with the new node.
      REAL, INTENT(IN) :: time !! Simulation time in hours.
      TYPE(LS), POINTER :: r !! Newly allocated node.
      TYPE(LS), POINTER :: prev_node !! Converted previous tail for a nonempty queue.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = 'FOR_NEW_TIME_LS'

      ALLOCATE (r, STAT=ios)
      CALL errstat_alloc(ios, "r", location)
      r%time = time

      ALLOCATE (r%s(ilow:ihigh, jlow:jhigh, klow:khigh, ext), STAT=ios)
      CALL errstat_alloc(ios, "r%s", location)
      r%s = default_real_edges

      IF (.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous => prev_node
         prev_node%next => r
      END IF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_LS

!> @brief Allocates and appends one real middle (`MS`) time node.
!>
!> Every middle value starts at `-1.0`; the payload retains the supplied
!> first-three-dimension bounds and uses `1:ext` for its fourth dimension.
!>
!> @warning
!> Allocation/bounds are unchecked. Non-null `first` requires `latest` to be a
!> live `MS` tail; a stale or mismatched handle is invalid.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added real middle time-node allocation and linking. |
!> | 2026-03-29 | SvB | Converted head/tail addresses to `C_PTR` and type-safe Fortran pointers. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE FOR_NEW_TIME_MS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN) :: ilow !! First column/list index.
      INTEGER, INTENT(IN) :: ihigh !! Last column/list index.
      INTEGER, INTENT(IN) :: jlow !! First row index.
      INTEGER, INTENT(IN) :: jhigh !! Last row index.
      INTEGER, INTENT(IN) :: klow !! First layer index.
      INTEGER, INTENT(IN) :: khigh !! Last layer index.
      INTEGER, INTENT(IN) :: ext !! Fourth-dimension upper bound.
      TYPE(C_PTR), INTENT(INOUT) :: first !! Head handle, set when the queue is empty.
      TYPE(C_PTR), INTENT(INOUT) :: latest !! Tail handle, replaced with the new node.
      REAL, INTENT(IN) :: time !! Simulation time in hours.
      TYPE(MS), POINTER :: r !! Newly allocated node.
      TYPE(MS), POINTER :: prev_node !! Converted previous tail for a nonempty queue.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = 'FOR_NEW_TIME_MS'

      ALLOCATE (r, STAT=ios)
      CALL errstat_alloc(ios, "r", location)
      r%time = time

      ALLOCATE (r%s(ilow:ihigh, jlow:jhigh, klow:khigh, ext), STAT=ios)
      CALL errstat_alloc(ios, "r%s", location)
      r%s = default_real_middle

      IF (.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous => prev_node
         prev_node%next => r
      END IF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_MS

!> @brief Allocates and appends one integer compound (`NS`) time node.
!>
!> Every square, bank, and river-link member starts at integer `-1`; the
!> payload retains the supplied first-three-dimension bounds and uses `1:ext`
!> for its fourth dimension.
!>
!> @warning
!> Allocation/bounds are unchecked. Non-null `first` requires `latest` to be a
!> live `NS` tail; a stale or mismatched handle is invalid.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2004-07 | JE | Added integer compound time-node allocation and linking. |
!> | 2026-03-29 | SvB | Converted head/tail addresses to `C_PTR` and type-safe Fortran pointers. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE FOR_NEW_TIME_NS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN) :: ilow !! First column/list index.
      INTEGER, INTENT(IN) :: ihigh !! Last column/list index.
      INTEGER, INTENT(IN) :: jlow !! First row index.
      INTEGER, INTENT(IN) :: jhigh !! Last row index.
      INTEGER, INTENT(IN) :: klow !! First layer index.
      INTEGER, INTENT(IN) :: khigh !! Last layer index.
      INTEGER, INTENT(IN) :: ext !! Fourth-dimension upper bound.
      TYPE(C_PTR), INTENT(INOUT) :: first !! Head handle, set when the queue is empty.
      TYPE(C_PTR), INTENT(INOUT) :: latest !! Tail handle, replaced with the new node.
      REAL, INTENT(IN) :: time !! Simulation time in hours.
      TYPE(NS), POINTER :: r !! Newly allocated node.
      TYPE(NS), POINTER :: prev_node !! Converted previous tail for a nonempty queue.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = 'FOR_NEW_TIME_NS'

      ALLOCATE (r, STAT=ios)
      CALL errstat_alloc(ios, "r", location)
      r%time = time

      ALLOCATE (r%s(ilow:ihigh, jlow:jhigh, klow:khigh, ext), STAT=ios)
      CALL errstat_alloc(ios, "r%s", location)
      r%s = default_integer_middle_and_edges

      IF (.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous => prev_node
         prev_node%next => r
      END IF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_NS
END MODULE visualisation_structure
