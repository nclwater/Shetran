!> @brief Coordinates metadata registration, state extraction, and visualisation output.
!>
!> This module is the runtime controller between the SHETRAN-facing catalogue
!> and accessors in [[visualisation_interface_centre]] and the metadata,
!> buffering, and HDF5 services re-exported by
!> [[visualisation_interface_far_right]]. Its only public procedure,
!> [[record_visualisation_data]], is called during simulation startup, after
!> every model timestep, and once with `text='end'` during final shutdown.
!> `north_order` is also public for compatibility, although it has no other
!> current source-tree consumer.
!>
!> The normal call sequence is:
!>
!> | Phase | Controller action |
!> |:------|:------------------|
!> | First startup call | Send paths and version through [[send_pass]], arm the HDF5 writer, and return. |
!> | Second startup call | Send geometry/topology, register catalogues, and record all items at time zero. |
!> | Timestep calls | Allocate and fill each due item's buffered node, then offer it to HDF5. |
!> | Final `end` call | Perform the ordinary due-item scan and then close visualisation resources. |
!>
!> Metadata's second-character `S` identifies the structure-backed storage
!> dispatched here. The first character determines value kind and member scope:
!>
!> | Type | Value and stored members | Filler |
!> |:-----|:-------------------------|:-------|
!> | `BS` | Real banks, four N/E/S/W edge slots | [[fill_b]] |
!> | `ES` | Integer banks, four N/E/S/W edge slots | [[fill_e]] |
!> | `FS` | Integer river links, four N/E/S/W edge slots | [[fill_f]] |
!> | `GS` | Real square plus four banks and four river links | [[fill_g]] |
!> | `IS` | Integer middle element only | [[fill_i]] |
!> | `LS` | Real river links, four N/E/S/W edge slots | [[fill_l]] |
!> | `MS` | Real middle element only | [[fill_m]] |
!> | `NS` | Integer middle plus four banks and four river links | [[fill_n]] |
!>
!> The current implemented catalogue and metadata transformations generate
!> `BS`, `GS`, `LS`, `MS`, and static `NS` items. The `ES`, `FS`, and `IS`
!> routes are retained legacy interfaces. All fillers call the elemental
!> centre accessors with a vector of bottom-up solver layers, producing one
!> value per stored visualisation layer.
!>
!> Coordinate and member translation is:
!>
!> | Stored convention | Translation used here |
!> |:------------------|:----------------------|
!> | Grid rows | [[shetran_j]] reverses HDF5/SHEGRAPH row `j` to the solver row. |
!> | Layers | `SHETRAN_LAYER` reverses top-down visualisation layers to bottom-up solver cells. |
!> | Face/member slots | `north_order=[north,east,south,west]` supplies solver face numbers in stored N/E/S/W order. |
!> | Other extra axes | `normal_order=[1,2,3,4]`; only the metadata-defined prefix is passed. |
!> | Inactive grid/list entry | Element zero is skipped, leaving the structure's initialized missing value. |
!>
!> The metadata items own opaque `C_PTR` handles to the first and latest
!> buffered nodes. [[record_visualisation_data]] copies them out, lets
!> `FOR_NEW_TIME` append a node, writes both handles back, fills the latest
!> node, and hands the item to `SAVE_VISUALISATION_DATA_TO_DISK`. The writer
!> consumes queued nodes when its one-value buffer is written.
!>
!> @warning
!> Startup and the downstream metadata/HDF5 modules use saved one-shot state;
!> this interface cannot safely initialize a second simulation in the same
!> process. Cleanup is requested only by the exact, case-sensitive text `end`.
!> A first-ever call carrying `end` would take the startup return before
!> cleanup.
!>
!> [[fill_select]] still routes `FS`, but `visualisation_structure:FOR_NEW_TIME`
!> has no active `FS` allocation branch. No current implemented catalogue item
!> generates `FS`; reactivating that type requires restoring its allocator.
!>
!> The middle writes in [[fill_g]], [[fill_i]], [[fill_m]], and [[fill_n]] pass
!> local `d` before it is assigned. The current `m` storage branches do not use
!> that dummy, so ordinary outputs do not index with the undefined value, but
!> the retained interface is fragile. Unsupported type strings have no
!> `CASE DEFAULT` and therefore perform no fill.
!> @endwarning
!>
!> @note
!> Fortran applies the source's bare `PRIVATE` statement to the whole module,
!> so only [[record_visualisation_data]] and `north_order` are public. The
!> current FORD parser applies default accessibility in source order and
!> consequently labels the parameters declared before `PRIVATE` as public in
!> its generated variable table. Their actual compiled accessibility is private.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Created the right-hand runtime interface for SHEGRAPH 2. |
!> | 2019-11-28 | SB | - | Imported the standard visualisation interface into the current repository. |
!> | 2026-04-04 | SvB | - | Applied the project-wide Fortran formatting pass. |
!> | 2026-04-08 | SB | 4.6.1 | Changed handles to `C_PTR` and removed the Intel real-kind directive. |
!> @endhistory
MODULE visualisation_interface_right

   USE ISO_C_BINDING, ONLY: C_PTR

   USE VISUALISATION_INTERFACE_CENTRE, ONLY: BANK_NO, ELEMENT, GRID_NX, GRID_NY, RIVER_NO, TOP_CELL, &
      IS_SQUARE, IS_BANK, IS_LINK, &
      north, east, south, west, EXISTS, NO_EL, csz, DIRQQ, &
      SHETRAN_INTEGER_DATA, SHETRAN_REAL_DATA, OUTPUT_TYPE, GET_OUTPUT_TYPE, &
      NO_SED, NO_CON, VERSION, ROOTDIR, SHETRAN_LAYER, &
      hdf5filename, planfile, checkfile
   USE VISUALISATION_INTERFACE_FAR_RIGHT, ONLY: G_C, G_L, G_I, S_PTR, G_PTR, &
      TIME_TO_RECORD, &
      REGISTER_STATIC_VISUALISATION_METADATA, &
      REGISTER_DYNAMIC_VISUALISATION_METADATA, &
      FOR_NEW_TIME, SAVE_ITEMS_WORTH, &
      SAVE_VISUALISATION_DATA_TO_DISK, VISUALISATION_TIDY_UP, &
      SEND_P

   USE MOD_PARAMETERS, ONLY: I_P
   USE MOD_ERROR, ONLY: errstat_alloc, errstat_dealloc

   IMPLICIT NONE

   INTEGER, DIMENSION(4), PARAMETER :: &
      north_order = (/north, east, south, west/) !! Solver face numbers in stored N/E/S/W order.
   INTEGER, DIMENSION(4), PARAMETER :: &
      normal_order = (/1, 2, 3, 4/)                !! Natural values for non-face extra dimensions.
   LOGICAL, PARAMETER :: T = .TRUE.  !! Compact true value used by the startup guards.
   LOGICAL, PARAMETER :: F = .FALSE. !! Compact false value used by the startup guards.

   REAL, PARAMETER :: zero = 0.0 !! Retained default-real zero; unused by current module procedures.

   PRIVATE
   PUBLIC :: RECORD_VISUALISATION_DATA, north_order

CONTAINS
!> @brief Registers and records visualisation data for one simulation time.
!>
!> Two saved guards make the first two calls special. The first calls
!> [[send_pass]] with selector 1, arms the HDF5 writer with its pre-registration
!> call, and returns. The second sends pass 2, registers allocated copies of
!> the static and dynamic centre catalogues, and then continues into the item
!> loop. Dynamic registration pass 1 writes the available-variable listing;
!> pass 2 reads and validates the user's plan and creates HDF5 metadata.
!>
!> `TIME_TO_RECORD` treats time zero as due for every registered item. For each
!> due item this routine reads its bounds, type, extra axis, mask/list state,
!> layer range, selected sediment/contaminant numbers, and opaque buffer
!> handles. `FOR_NEW_TIME` appends a default-initialized node, after which
!> [[fill_select]] replaces values for active grid cells or nonzero list
!> entries. Skipped cells retain the structure's missing defaults. The item is
!> then offered to the HDF5 writer.
!>
!> Grid iteration uses stored/display coordinates `(i,j)` for masks and buffer
!> positions, native row `jj=SHETRAN_J(j)` for centre accessors, and
!> [[su_number]] for the element. List iteration stores at `(nn,1)`, obtains
!> the element from metadata, and uses native-row placeholder 1; current list
!> selectors do not require grid-derived coordinates.
!>
!> `time` is default-real simulation time in hours. Optional `text` has no
!> effect except that exact `end` triggers `VISUALISATION_TIDY_UP` after the
!> due-item loop.
!>
!> @warning
!> The saved startup guards and downstream allocated state make this a
!> process-lifetime, one-shot controller. It is not thread-safe or restartable
!> for a second run in the same process.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added startup registration, scheduled filling, writing, and shutdown. |
!> | 2026-04-08 | SB | 4.6.1 | Replaced stored integer addresses with interoperable `C_PTR` handles. |
!> @endhistory
   SUBROUTINE record_visualisation_data(time, text)
      REAL, INTENT(IN)                         :: time   !! Simulation time (hours).
      CHARACTER(*), INTENT(IN), OPTIONAL       :: text   !! Exact `end` requests cleanup after recording.
      INTEGER                                  :: i      !! Catalogue or stored x/list index.
      INTEGER                                  :: j      !! Stored HDF5/SHEGRAPH row index.
      INTEGER                                  :: jj     !! Native SHETRAN row corresponding to `j`.
      INTEGER                                  :: k      !! Dynamic-registration pass number.
      INTEGER                                  :: mn     !! Registered metadata-item index.
      INTEGER                                  :: nn     !! Non-grid list-position index.
      INTEGER                                  :: su     !! Current SHETRAN element number.
      INTEGER                                  :: ilow   !! Lower stored x/list bound.
      INTEGER                                  :: ihigh  !! Upper stored x/list bound.
      INTEGER                                  :: jlow   !! Lower stored row bound.
      INTEGER                                  :: jhigh  !! Upper stored row bound.
      INTEGER                                  :: klow   !! Lower top-down visualisation-layer bound.
      INTEGER                                  :: khigh  !! Upper top-down visualisation-layer bound.
      INTEGER                                  :: sz     !! Number of entries in a non-grid item list.
      INTEGER                                  :: ext    !! Conceptual extra-axis extent; at least one.
      INTEGER                                  :: nsed   !! Selected sediment-fraction number.
      INTEGER                                  :: ncon   !! Selected contaminant number.
      INTEGER                                  :: n      !! Implied-DO index used to construct layer numbers.
      TYPE(C_PTR)                              :: first  !! Head of the item's buffered-node chain.
      TYPE(C_PTR)                              :: latest !! Tail of the item's buffered-node chain.
      LOGICAL                                  :: isgrid !! Whether the item uses stored grid coordinates.
      INTEGER, DIMENSION(4)                    :: ee     !! Solver-facing values for the extra axis.
      CHARACTER(2)                             :: typ    !! Structure storage code dispatched by [[fill_select]].
      CHARACTER(8)                             :: ext_dim !! Metadata name of the extra dimension.
      CHARACTER(csz)                           :: name   !! Exact centre-accessor selector name.
      LOGICAL, SAVE                            :: one = T  !! First-call guard.
      LOGICAL, SAVE                            :: two = F  !! Second-call registration guard.
      TYPE(OUTPUT_TYPE), DIMENSION(:), POINTER :: oty    !! Allocated static or dynamic catalogue copy.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_INTERFACE_RIGHT:record_visualisation_data"

      IF (one) THEN
         one = F
         two = T
         CALL SEND_PASS(1)
         CALL SAVE_VISUALISATION_DATA_TO_DISK(1, 0.0)
         RETURN
      ELSEIF (two) THEN
         two = F
         CALL SEND_PASS(2)
         oty => GET_OUTPUT_TYPE('static')
         DO i = LBOUND(oty, DIM=1), UBOUND(oty, DIM=1)
            CALL REGISTER_STATIC_VISUALISATION_METADATA(oty(i)%name, oty(i)%typ, &
               oty(i)%units, oty(i)%title, GRID_NX(), GRID_NY(), oty(i)%extra_dimensions, oty(i)%varies_with_elevation)
         END DO
         DEALLOCATE (oty, STAT=ios)
         CALL errstat_dealloc(ios, "oty", location)
         oty => GET_OUTPUT_TYPE('dynamic')
         DO k = 1, 2
            DO i = LBOUND(oty, DIM=1), UBOUND(oty, DIM=1)
               CALL REGISTER_DYNAMIC_VISUALISATION_METADATA(k, i == SIZE(oty) .AND. k == 2, oty(i)%name, oty(i)%typ, &
                  oty(i)%units, oty(i)%title, oty(i)%extra_dimensions, oty(i)%varies_with_elevation, &
                  oty(i)%varies_with_sediment_no, oty(i)%varies_with_contaminant_no, &
                  oty(i)%implemented)
            END DO
         END DO
         DEALLOCATE (oty, STAT=ios)
         CALL errstat_dealloc(ios, "oty", location)
      END IF

      MM: DO mn = 1, G_I(0, 'no_items')

         IF (.NOT. TIME_TO_RECORD(mn, time)) CYCLE MM
         name = G_C(mn, 'name')
         typ = G_C(mn, 'typ')
         ilow = G_I(mn, 'ilow')
         ihigh = G_I(mn, 'ihigh')
         jlow = G_I(mn, 'jlow')
         jhigh = G_I(mn, 'jhigh')
         klow = G_I(mn, 'klow')
         khigh = G_I(mn, 'khigh')
         isgrid = G_L(mn, 'isgrid')
         ext = G_I(mn, 'ext')
         IF (ext > 0) THEN
            ext_dim = G_C(mn, 'extra_dimensions')
            IF (ext_dim == 'faces') THEN
               ee = north_order
            ELSE
               ee = normal_order
            END IF
         END IF
         first = G_PTR(mn, 'first')
         latest = G_PTR(mn, 'latest')
         nsed = G_I(mn, 'nsed')
         ncon = G_I(mn, 'ncon')
         CALL FOR_NEW_TIME(typ, time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
         CALL S_PTR(mn, 'first', first)
         CALL S_PTR(mn, 'latest', latest)
         IF (.NOT. isgrid) THEN
            sz = G_I(mn, 'sz')
            DO nn = 1, sz
               su = G_I(mn, 'su', nn)
               IF (su == 0) CYCLE  ! Leave missing list entries at structure defaults.
               CALL FILL_SELECT(name, typ, nn, 1, 1, su, klow, khigh, &
                  SHETRAN_LAYER((/(n, n=klow, khigh)/)), ee(1:ext), latest, nsed=nsed, ncon=ncon)
            END DO
         ELSE
            DO i = ilow, ihigh
               DO j = jlow, jhigh
                  jj = SHETRAN_J(j)  ! Convert the display row to native SHETRAN orientation.
                  IF (.NOT. G_L(mn, 'on', i, j)) CYCLE
                  su = SU_NUMBER(i, j)
                  IF (su == 0) CYCLE  ! Leave cells outside the catchment at structure defaults.
                  CALL FILL_SELECT(name, typ, i, j, jj, su, klow, khigh, &
                     SHETRAN_LAYER((/(n, n=klow, khigh)/)), ee(1:ext), latest, nsed=nsed, ncon=ncon)
               END DO
            END DO
         END IF
         CALL SAVE_VISUALISATION_DATA_TO_DISK(mn, time)
      END DO MM

      IF (PRESENT(text)) THEN
         IF (text == 'end') CALL VISUALISATION_TIDY_UP()
      END IF
   END SUBROUTINE record_visualisation_data

!> @brief Dispatches one element/list position to its structure-specific filler.
!>
!> The exact two-character `typ` is mapped to one of the eight fillers listed
!> in the module-level table. All indices, remapped layers, extra-axis values,
!> selector numbers, and the latest-node handle are passed through unchanged.
!> There is no default branch, error, or status result for an unsupported type.
!>
!> @warning `FS` is dispatched here, but the current structure module has no
!> active allocator for a new `FS` time node.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added dispatch for the eight SHEGRAPH structure types. |
!> | 2026-04-08 | SB | 4.6.1 | Changed the latest-node argument from an integer address to `C_PTR`. |
!> @endhistory
   SUBROUTINE fill_select(name, typ, a, b, bb, su, klow, khigh, silay, ee, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name   !! Exact centre-accessor selector name.
      CHARACTER(*), INTENT(IN)          :: typ    !! Two-character structure storage code.
      INTEGER, INTENT(IN)               :: a      !! Stored x index or list position.
      INTEGER, INTENT(IN)               :: b      !! Stored display-row index; one for lists.
      INTEGER, INTENT(IN)               :: bb     !! Native solver row; one for lists.
      INTEGER, INTENT(IN)               :: su     !! Current SHETRAN element number.
      INTEGER, INTENT(IN)               :: klow   !! Lower stored visualisation-layer bound.
      INTEGER, INTENT(IN)               :: khigh  !! Upper stored visualisation-layer bound.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay  !! Bottom-up solver cells for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee     !! Solver-facing extra-axis values.
      TYPE(C_PTR), INTENT(IN)           :: latest !! Latest allocated structure node.
      INTEGER, INTENT(IN)               :: nsed   !! Selected sediment-fraction number.
      INTEGER, INTENT(IN)               :: ncon   !! Selected contaminant number.
      SELECT CASE (typ)
       CASE ('BS'); CALL FILL_B(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
       CASE ('ES'); CALL FILL_E(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
       CASE ('FS'); CALL FILL_F(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
       CASE ('GS'); CALL FILL_G(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
       CASE ('IS'); CALL FILL_I(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
       CASE ('LS'); CALL FILL_L(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
       CASE ('MS'); CALL FILL_M(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
       CASE ('NS'); CALL FILL_N(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      END SELECT
   END SUBROUTINE fill_select

!> @brief Fills real-valued bank members around one gridsquare.
!>
!> `BANK_NO(su,north_order)` produces four bank references in stored N/E/S/W
!> slots. Missing banks remain at their initialized defaults. Each existing
!> bank is evaluated for every extra-axis value and all requested solver cells,
!> then stored in edge member `d` of the latest `BS` node.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added real bank-member filling. |
!> | 2026-04-08 | SB | 4.6.1 | Changed the latest-node argument to `C_PTR`. |
!> @endhistory
   SUBROUTINE fill_b(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name     !! Exact real-accessor selector name.
      INTEGER, INTENT(IN)               :: a        !! Stored x index or list position.
      INTEGER, INTENT(IN)               :: b        !! Stored display-row index.
      INTEGER, INTENT(IN)               :: bb       !! Native solver row.
      INTEGER, INTENT(IN)               :: su       !! Gridsquare element used for bank lookup.
      INTEGER, INTENT(IN)               :: klow     !! Lower stored visualisation-layer bound.
      INTEGER, INTENT(IN)               :: khigh    !! Upper stored visualisation-layer bound.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay    !! Bottom-up solver cells for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee       !! Solver-facing extra-axis values.
      CHARACTER(*), INTENT(IN)          :: typ      !! Expected storage type `BS`.
      TYPE(C_PTR), INTENT(IN)           :: latest   !! Latest allocated `BS` node.
      INTEGER, INTENT(IN)               :: nsed     !! Selected sediment-fraction number.
      INTEGER, INTENT(IN)               :: ncon     !! Selected contaminant number.
      INTEGER                           :: d        !! Stored face/member slot in N/E/S/W order.
      INTEGER                           :: e        !! Extra-axis position.
      INTEGER                           :: banks(4) !! Bank elements in stored N/E/S/W order.
      banks = BANK_NO(su, north_order)
      DO d = 1, 4
         IF (EXISTS(banks(d))) THEN
            DO e = 1, SIZE(ee)
               CALL SAVE_ITEMS_WORTH('e', typ, a, b, klow, khigh, e, d, &
                  SHETRAN_REAL_DATA(name, banks(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
            END DO
         END IF
      END DO
   END SUBROUTINE fill_b

!> @brief Fills integer-valued bank members around one gridsquare.
!>
!> This is the integer analogue of [[fill_b]]. Existing N/E/S/W banks are
!> evaluated through `SHETRAN_INTEGER_DATA` and stored in an `ES` edge member.
!> `nsed` and `ncon` are retained in the common signature but are not passed to
!> the integer accessor; its current selectors do not use them.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added integer bank-member filling. |
!> | 2026-04-08 | SB | 4.6.1 | Changed the latest-node argument to `C_PTR`. |
!> @endhistory
   SUBROUTINE fill_e(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name     !! Exact integer-accessor selector name.
      INTEGER, INTENT(IN)               :: a        !! Stored x index or list position.
      INTEGER, INTENT(IN)               :: b        !! Stored display-row index.
      INTEGER, INTENT(IN)               :: bb       !! Native solver row.
      INTEGER, INTENT(IN)               :: su       !! Gridsquare element used for bank lookup.
      INTEGER, INTENT(IN)               :: klow     !! Lower stored visualisation-layer bound.
      INTEGER, INTENT(IN)               :: khigh    !! Upper stored visualisation-layer bound.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay    !! Bottom-up solver cells for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee       !! Solver-facing extra-axis values.
      CHARACTER(*), INTENT(IN)          :: typ      !! Expected storage type `ES`.
      TYPE(C_PTR), INTENT(IN)           :: latest   !! Latest allocated `ES` node.
      INTEGER, INTENT(IN)               :: nsed     !! Retained common sediment selector; unused here.
      INTEGER, INTENT(IN)               :: ncon     !! Retained common contaminant selector; unused here.
      INTEGER                           :: d        !! Stored face/member slot in N/E/S/W order.
      INTEGER                           :: e        !! Extra-axis position.
      INTEGER                           :: banks(4) !! Bank elements in stored N/E/S/W order.
      banks = BANK_NO(su, north_order)
      DO d = 1, 4
         IF (EXISTS(banks(d))) THEN
            DO e = 1, SIZE(ee)
               CALL SAVE_ITEMS_WORTH('e', typ, a, b, klow, khigh, e, d, &
                  SHETRAN_INTEGER_DATA(name, banks(d), ix=a, iy=bb, ilay=silay, ext=ee(e)), latest)
            END DO
         END IF
      END DO
   END SUBROUTINE fill_e

!> @brief Fills integer-valued river-link members around one gridsquare.
!>
!> Existing link references from `RIVER_NO(su,north_order)` are evaluated for
!> every extra-axis value and solver layer, then stored in N/E/S/W `FS` edge
!> slots. Missing links retain the structure defaults.
!>
!> @warning The corresponding `FS` node allocator is disabled in the current
!> structure module, so this retained route is unsafe if reactivated.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added integer river-link filling. |
!> | 2026-04-08 | SB | 4.6.1 | Changed the latest-node argument to `C_PTR`. |
!> @endhistory
   SUBROUTINE fill_f(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name      !! Exact integer-accessor selector name.
      INTEGER, INTENT(IN)               :: a         !! Stored x index or list position.
      INTEGER, INTENT(IN)               :: b         !! Stored display-row index.
      INTEGER, INTENT(IN)               :: bb        !! Native solver row.
      INTEGER, INTENT(IN)               :: su        !! Gridsquare element used for link lookup.
      INTEGER, INTENT(IN)               :: klow      !! Lower stored visualisation-layer bound.
      INTEGER, INTENT(IN)               :: khigh     !! Upper stored visualisation-layer bound.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay     !! Bottom-up solver cells for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee        !! Solver-facing extra-axis values.
      CHARACTER(*), INTENT(IN)          :: typ       !! Expected storage type `FS`.
      TYPE(C_PTR), INTENT(IN)           :: latest    !! Latest allocated `FS` node.
      INTEGER, INTENT(IN)               :: nsed      !! Selected sediment-fraction number.
      INTEGER, INTENT(IN)               :: ncon      !! Selected contaminant number.
      INTEGER                           :: d         !! Stored face/member slot in N/E/S/W order.
      INTEGER                           :: e         !! Extra-axis position.
      INTEGER                           :: rivers(4) !! Link elements in stored N/E/S/W order.
      rivers = RIVER_NO(su, north_order)
      DO d = 1, 4
         IF (EXISTS(rivers(d))) THEN
            DO e = 1, SIZE(ee)
               CALL SAVE_ITEMS_WORTH('e', typ, a, b, klow, khigh, e, d, &
                  SHETRAN_INTEGER_DATA(name, rivers(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
            END DO
         END IF
      END DO
   END SUBROUTINE fill_f

!> @brief Fills a real compound of one gridsquare, its banks, and its links.
!>
!> For every extra-axis position the gridsquare value is written to member
!> `m`. The routine then resolves banks and river links in stored N/E/S/W order
!> and writes existing elements to members `b(d)` and `r(d)` respectively.
!> Missing edge elements retain their initialized -1.0 defaults.
!>
!> @warning Local `d` is undefined when passed with the initial middle write.
!> `SAVE_GS` selects member `m` for marker `m` and does not use `d` in that
!> branch; this relies on the downstream implementation continuing to ignore it.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added compound real square/bank/link filling. |
!> | 2026-04-08 | SB | 4.6.1 | Changed the latest-node argument to `C_PTR`. |
!> @endhistory
   SUBROUTINE fill_g(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name      !! Exact real-accessor selector name.
      INTEGER, INTENT(IN)               :: a         !! Stored x index or list position.
      INTEGER, INTENT(IN)               :: b         !! Stored display-row index.
      INTEGER, INTENT(IN)               :: bb        !! Native solver row.
      INTEGER, INTENT(IN)               :: su        !! Gridsquare element at the compound centre.
      INTEGER, INTENT(IN)               :: klow      !! Lower stored visualisation-layer bound.
      INTEGER, INTENT(IN)               :: khigh     !! Upper stored visualisation-layer bound.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay     !! Bottom-up solver cells for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee        !! Solver-facing extra-axis values.
      CHARACTER(*), INTENT(IN)          :: typ       !! Expected storage type `GS`.
      TYPE(C_PTR), INTENT(IN)           :: latest    !! Latest allocated `GS` node.
      INTEGER, INTENT(IN)               :: nsed      !! Selected sediment-fraction number.
      INTEGER, INTENT(IN)               :: ncon      !! Selected contaminant number.
      INTEGER                           :: d         !! N/E/S/W member slot; undefined during middle writes.
      INTEGER                           :: e         !! Extra-axis position.
      INTEGER                           :: banks(4)  !! Bank elements in stored N/E/S/W order.
      INTEGER                           :: rivers(4) !! Link elements in stored N/E/S/W order.
      DO e = 1, SIZE(ee)
         CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
            SHETRAN_REAL_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
      END DO
      rivers = RIVER_NO(su, north_order)
      banks = BANK_NO(su, north_order)
      DO d = 1, 4
         IF (EXISTS(banks(d))) THEN
            DO e = 1, SIZE(ee)
               CALL SAVE_ITEMS_WORTH('b', typ, a, b, klow, khigh, e, d, &
                  SHETRAN_REAL_DATA(name, banks(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
            END DO
         END IF
         IF (EXISTS(rivers(d))) THEN
            DO e = 1, SIZE(ee)
               CALL SAVE_ITEMS_WORTH('r', typ, a, b, klow, khigh, e, d, &
                  SHETRAN_REAL_DATA(name, rivers(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
            END DO
         END IF
      END DO
   END SUBROUTINE fill_g

!> @brief Fills an integer middle-element value.
!>
!> The current element is evaluated for each extra-axis position and every
!> requested solver layer, then stored in member `m` of the latest `IS` node.
!> For a non-grid list, `su` is the listed element and need not be a gridsquare.
!>
!> @warning `d` is passed undefined, but `SAVE_IS` does not accept or use a
!> member-slot argument. Local `n` is retained but unused.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added integer middle-element filling. |
!> | 2026-04-08 | SB | 4.6.1 | Changed the latest-node argument to `C_PTR`. |
!> @endhistory
   SUBROUTINE fill_i(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name   !! Exact integer-accessor selector name.
      INTEGER, INTENT(IN)               :: a      !! Stored x index or list position.
      INTEGER, INTENT(IN)               :: b      !! Stored display-row index.
      INTEGER, INTENT(IN)               :: bb     !! Native solver row; one for lists.
      INTEGER, INTENT(IN)               :: su     !! Element stored as the middle member.
      INTEGER, INTENT(IN)               :: klow   !! Lower stored visualisation-layer bound.
      INTEGER, INTENT(IN)               :: khigh  !! Upper stored visualisation-layer bound.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay  !! Bottom-up solver cells for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee     !! Solver-facing extra-axis values.
      CHARACTER(*), INTENT(IN)          :: typ    !! Expected storage type `IS`.
      TYPE(C_PTR), INTENT(IN)           :: latest !! Latest allocated `IS` node.
      INTEGER, INTENT(IN)               :: nsed   !! Selected sediment-fraction number.
      INTEGER, INTENT(IN)               :: ncon   !! Selected contaminant number.
      INTEGER                           :: d      !! Undefined slot passed through but ignored by `IS` storage.
      INTEGER                           :: e      !! Extra-axis position.
      INTEGER                           :: n      !! Retained legacy work index; unused.
      DO e = 1, SIZE(ee)
         CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
            SHETRAN_INTEGER_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
      END DO
   END SUBROUTINE fill_i

!> @brief Fills real-valued river-link members around one gridsquare.
!>
!> Existing link references from `RIVER_NO(su,north_order)` are evaluated for
!> every extra-axis value and solver layer and stored in N/E/S/W `LS` edge
!> slots. Missing links retain the structure defaults.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added real river-link filling. |
!> | 2026-04-08 | SB | 4.6.1 | Changed the latest-node argument to `C_PTR`. |
!> @endhistory
   SUBROUTINE fill_L(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name      !! Exact real-accessor selector name.
      INTEGER, INTENT(IN)               :: a         !! Stored x index or list position.
      INTEGER, INTENT(IN)               :: b         !! Stored display-row index.
      INTEGER, INTENT(IN)               :: bb        !! Native solver row.
      INTEGER, INTENT(IN)               :: su        !! Gridsquare element used for link lookup.
      INTEGER, INTENT(IN)               :: klow      !! Lower stored visualisation-layer bound.
      INTEGER, INTENT(IN)               :: khigh     !! Upper stored visualisation-layer bound.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay     !! Bottom-up solver cells for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee        !! Solver-facing extra-axis values.
      CHARACTER(*), INTENT(IN)          :: typ       !! Expected storage type `LS`.
      TYPE(C_PTR), INTENT(IN)           :: latest    !! Latest allocated `LS` node.
      INTEGER, INTENT(IN)               :: nsed      !! Selected sediment-fraction number.
      INTEGER, INTENT(IN)               :: ncon      !! Selected contaminant number.
      INTEGER                           :: d         !! Stored face/member slot in N/E/S/W order.
      INTEGER                           :: e         !! Extra-axis position.
      INTEGER                           :: rivers(4) !! Link elements in stored N/E/S/W order.
      rivers = RIVER_NO(su, north_order)
      DO d = 1, 4
         IF (EXISTS(rivers(d))) THEN
            DO e = 1, SIZE(ee)
               CALL SAVE_ITEMS_WORTH('e', typ, a, b, klow, khigh, e, d, &
                  SHETRAN_REAL_DATA(name, rivers(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
            END DO
         END IF
      END DO
   END SUBROUTINE fill_L

!> @brief Fills a real middle-element value.
!>
!> The current element is evaluated for each extra-axis position and every
!> requested solver layer and stored in member `m` of the latest `MS` node.
!> This is the normal type used for a square-only grid scope and for real list
!> items, where `su` may instead be any listed square, bank, or link.
!>
!> @warning `d` is passed undefined, but `SAVE_MS` does not accept or use a
!> member-slot argument. Local `n` is retained but unused.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added real middle-element filling. |
!> | 2026-04-08 | SB | 4.6.1 | Changed the latest-node argument to `C_PTR`. |
!> @endhistory
   SUBROUTINE fill_m(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name   !! Exact real-accessor selector name.
      INTEGER, INTENT(IN)               :: a      !! Stored x index or list position.
      INTEGER, INTENT(IN)               :: b      !! Stored display-row index.
      INTEGER, INTENT(IN)               :: bb     !! Native solver row; one for lists.
      INTEGER, INTENT(IN)               :: su     !! Element stored as the middle member.
      INTEGER, INTENT(IN)               :: klow   !! Lower stored visualisation-layer bound.
      INTEGER, INTENT(IN)               :: khigh  !! Upper stored visualisation-layer bound.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay  !! Bottom-up solver cells for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee     !! Solver-facing extra-axis values.
      CHARACTER(*), INTENT(IN)          :: typ    !! Expected storage type `MS`.
      TYPE(C_PTR), INTENT(IN)           :: latest !! Latest allocated `MS` node.
      INTEGER, INTENT(IN)               :: nsed   !! Selected sediment-fraction number.
      INTEGER, INTENT(IN)               :: ncon   !! Selected contaminant number.
      INTEGER                           :: d      !! Undefined slot passed through but ignored by `MS` storage.
      INTEGER                           :: e      !! Extra-axis position.
      INTEGER                           :: n      !! Retained legacy work index; unused.
      DO e = 1, SIZE(ee)
         CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
            SHETRAN_REAL_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
      END DO
   END SUBROUTINE fill_m

!> @brief Fills an integer compound of one gridsquare, its banks, and its links.
!>
!> This is the integer analogue of [[fill_g]]. It writes the centre element to
!> member `m`, then existing N/E/S/W banks and links to members `b(d)` and
!> `r(d)`. Missing edge elements retain their initialized -1 defaults.
!>
!> @warning Local `d` is undefined when passed with the initial middle write.
!> `SAVE_NS` selects member `m` for marker `m` and does not use `d` in that
!> branch; this relies on the downstream implementation continuing to ignore it.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added compound integer square/bank/link filling. |
!> | 2026-04-08 | SB | 4.6.1 | Changed the latest-node argument to `C_PTR`. |
!> @endhistory
   SUBROUTINE fill_n(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name      !! Exact integer-accessor selector name.
      INTEGER, INTENT(IN)               :: a         !! Stored x index or list position.
      INTEGER, INTENT(IN)               :: b         !! Stored display-row index.
      INTEGER, INTENT(IN)               :: bb        !! Native solver row.
      INTEGER, INTENT(IN)               :: su        !! Gridsquare element at the compound centre.
      INTEGER, INTENT(IN)               :: klow      !! Lower stored visualisation-layer bound.
      INTEGER, INTENT(IN)               :: khigh     !! Upper stored visualisation-layer bound.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay     !! Bottom-up solver cells for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee        !! Solver-facing extra-axis values.
      CHARACTER(*), INTENT(IN)          :: typ       !! Expected storage type `NS`.
      TYPE(C_PTR), INTENT(IN)           :: latest    !! Latest allocated `NS` node.
      INTEGER, INTENT(IN)               :: nsed      !! Selected sediment-fraction number.
      INTEGER, INTENT(IN)               :: ncon      !! Selected contaminant number.
      INTEGER                           :: d         !! N/E/S/W member slot; undefined during middle writes.
      INTEGER                           :: e         !! Extra-axis position.
      INTEGER                           :: banks(4)  !! Bank elements in stored N/E/S/W order.
      INTEGER                           :: rivers(4) !! Link elements in stored N/E/S/W order.
      DO e = 1, SIZE(ee)
         CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
            SHETRAN_INTEGER_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
      END DO
      rivers = RIVER_NO(su, north_order)
      banks = BANK_NO(su, north_order)
      DO d = 1, 4
         IF (EXISTS(banks(d))) THEN
            DO e = 1, SIZE(ee)
               CALL SAVE_ITEMS_WORTH('b', typ, a, b, klow, khigh, e, d, &
                  SHETRAN_INTEGER_DATA(name, banks(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
            END DO
         END IF
         IF (EXISTS(rivers(d))) THEN
            DO e = 1, SIZE(ee)
               CALL SAVE_ITEMS_WORTH('r', typ, a, b, klow, khigh, e, d, &
                  SHETRAN_INTEGER_DATA(name, rivers(d), ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
            END DO
         END IF
      END DO
   END SUBROUTINE fill_n

!> @brief Copies startup paths, dimensions, topology, and type masks downstream.
!>
!> The selector controls two one-shot passes:
!>
!> | `jj` | Values sent through `SEND_P` |
!> |:----:|:-----------------------------|
!> | 1 | Run/output directories, HDF5/plan/check filenames, and integer major version. |
!> | 2 | Face constants, sizes/counts, type masks, display element grid, and topology tables. |
!>
!> Pass 2 constructs `iel=1:NO_EL()`. [[su_number]] reverses each output-grid
!> row before constructing the `nx` by `ny` element grid. The bank and link
!> tables have shape `(NO_EL(),4)` and retain native face-column order
!> E/N/W/S; non-gridsquare rows are zero. Downstream `visualisation_map` uses
!> the sent native face constants when indexing those columns.
!>
!> Temporary allocatables are released explicitly or automatically on return,
!> while `SEND_P` allocates persistent copies in `visualisation_pass`.
!> Selectors other than 1 or 2 perform no action.
!>
!> @warning Pass 2 is one-shot because `SEND_P` allocates its destination
!> arrays without first deallocating them.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added the two-stage transfer into the far-right visualisation state. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE send_pass(jj)
      INTEGER, INTENT(IN)                  :: jj !! Setup selector: 1 for paths, 2 for topology.
      INTEGER                              :: i  !! Element or native grid-column constructor index.
      INTEGER                              :: j  !! Native face or display-row constructor index.
      INTEGER                              :: nx !! Number of native grid columns.
      INTEGER                              :: ny !! Number of native grid rows.
      INTEGER                              :: total_no_elements !! Dynamic active-element count.
      INTEGER, DIMENSION(:), ALLOCATABLE   :: iel !! Element vector `1:total_no_elements`.
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: dum !! Temporary grid or topology table copied by `SEND_P`.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_INTERFACE_RIGHT:send_pass"

      SELECT CASE (jj)
       CASE (1)
         CALL SEND_P('dirqq', cc=dirqq, da=0, db=0)
         CALL SEND_P('rootdir', cc=rootdir, da=0, db=0)
         CALL SEND_p('ver', ii=VERSION(), da=0, db=0)
         CALL SEND_p('hdf5fname', cc=hdf5filename, da=0, db=0)
         CALL SEND_p('planfile', cc=planfile, da=0, db=0)
         CALL SEND_p('checkfile', cc=checkfile, da=0, db=0)
       CASE (2)
         total_no_elements = NO_EL()
         ALLOCATE (iel(total_no_elements), STAT=ios)
         CALL errstat_alloc(ios, "iel", location)
         iel = (/(i, i=1, total_no_elements)/)
         nx = GRID_NX()
         ny = GRID_NY()
         CALL SEND_P('north', ii=north, da=0, db=0)
         CALL SEND_P('east', ii=east, da=0, db=0)
         CALL SEND_P('south', ii=south, da=0, db=0)
         CALL SEND_P('west', ii=west, da=0, db=0)
         CALL SEND_P('grid_nx', ii=nx, da=0, db=0)
         CALL SEND_P('grid_ny', ii=ny, da=0, db=0)
         CALL SEND_P('top_cell', ii=TOP_CELL(), da=0, db=0)
         CALL SEND_P('nel', ii=total_no_elements, da=0, db=0)
         CALL SEND_P('nsed', ii=NO_SED(), da=0, db=0)
         CALL SEND_P('ncon', ii=NO_CON(), da=0, db=0)
         CALL SEND_P('is_square', L1=IS_SQUARE(iel), da=total_no_elements, db=0)
         CALL SEND_P('is_bank', L1=IS_BANK(iel), da=total_no_elements, db=0)
         CALL SEND_P('is_link', L1=IS_LINK(iel), da=total_no_elements, db=0)
         ALLOCATE (dum(nx, ny), STAT=ios)
         CALL errstat_alloc(ios, "dum", location)
         DO i = 1, nx; dum(i, :) = SU_NUMBER(i, (/(j, j=1, ny)/))
         END DO
         CALL SEND_P('su', d2=dum, da=nx, db=ny)
         DEALLOCATE (dum, STAT=ios)
         CALL errstat_dealloc(ios, "dum", location)
         ALLOCATE (dum(total_no_elements, 4), STAT=ios)
         CALL errstat_alloc(ios, "dum", location)
         DO j = 1, 4
            WHERE (IS_SQUARE(iel)); dum(:, j) = BANK_NO(iel, j); ELSEWHERE; dum(:, j) = 0; END WHERE
         END DO
         CALL SEND_P('bank_no', d2=dum, da=total_no_elements, db=4)
         DO j = 1, 4
            WHERE (IS_SQUARE(iel)); dum(:, j) = RIVER_NO(iel, j); ELSEWHERE; dum(:, j) = 0; END WHERE
         END DO
         CALL SEND_P('river_no', d2=dum, da=total_no_elements, db=4)
         DEALLOCATE (dum, STAT=ios)
         CALL errstat_dealloc(ios, "dum", location)
      END SELECT
   END SUBROUTINE send_pass

!> @brief Maps stored visualisation grid coordinates to a SHETRAN element.
!>
!> The x index is unchanged and [[shetran_j]] reverses the stored y index
!> before `ELEMENT` queries the native `ICMXY` grid. An inactive catchment cell
!> therefore returns its zero element reference. Both indices must lie inside
!> the configured grid.
!>
!> Returns the SHETRAN element number, or zero for an inactive grid cell.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added display-grid to solver-element translation. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION su_number(i, j) RESULT(r)
      INTEGER, INTENT(IN) :: i !! Stored HDF5/SHEGRAPH column index.
      INTEGER, INTENT(IN) :: j !! Stored HDF5/SHEGRAPH row index.
      r = ELEMENT(i, SHETRAN_J(j))
   END FUNCTION su_number

!> @brief Reverses a stored visualisation row into native SHETRAN orientation.
!>
!> The mapping is its own inverse over the valid domain 1:`GRID_NY()`:
!>
!> \[
!> r = GRID\_NY() - sgv2j + 1
!> \]
!>
!> No range check is performed; an out-of-range argument produces an equally
!> out-of-range reflected result.
!>
!> Returns the native SHETRAN row index.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2004-07 | JE | 2.0 | Added row-orientation reversal for SHEGRAPH 2. |
!> @endhistory
   ELEMENTAL INTEGER FUNCTION shetran_j(sgv2j) RESULT(r)
      INTEGER, INTENT(IN) :: sgv2j !! Stored HDF5/SHEGRAPH row index.
      r = GRID_NY() - sgv2j + 1
   END FUNCTION shetran_j

END MODULE visualisation_interface_right
