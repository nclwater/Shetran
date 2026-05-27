!> summary: Right-hand visualisation data recorder.
!>
!> This module coordinates visualisation output at run time. It sends static
!> geometry and file metadata to the far-right/output modules, registers
!> available variables, allocates new time slices when output is due, fills the
!> requested data from the central SHETRAN accessor layer, and asks the HDF5
!> writer to persist each item.
!>
!> Runtime sequence:
!>
!> | Call | Action |
!> |:-----|:-------|
!> | First call | Send directory/file metadata, initialise the writer, and return. |
!> | Second call | Send geometry metadata and register static and dynamic variables. |
!> | Later calls | If `TIME_TO_RECORD` is true for an item, allocate a new slice, fill it, and write it. |
!> | `text='end'` | Close visualisation output resources. |
!>
!> Coordinate and extra-dimension remapping:
!>
!> | Interface convention | SHETRAN convention used for accessor calls |
!> |:---------------------|:------------------------------------------|
!> | HDF5/SHEGRAPH face order `N,E,S,W` | `north_order = [north,east,south,west]`. |
!> | Non-face extra dimensions | `normal_order = [1,2,3,4]`. |
!> | HDF5/SHEGRAPH grid row `j` | `SHETRAN_J(j)=GRID_NY()-j+1`. |
!> | HDF5/SHEGRAPH layer `k` | `SHETRAN_LAYER(k)` from [[visualisation_interface_centre]]. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 200407 | JE | 2.0 | Created for SHEGRAPH Version 2. |
!> @endhistory
MODULE visualisation_interface_right

   USE ISO_C_BINDING, ONLY: C_PTR

   USE VISUALISATION_INTERFACE_CENTRE,    ONLY : BANK_NO, ELEMENT, GRID_NX, GRID_NY, RIVER_NO, TOP_CELL, &
      IS_SQUARE, IS_BANK, IS_LINK,                            &
      north, east, south, west, EXISTS, NO_EL, csz, DIRQQ,    &
      SHETRAN_INTEGER_DATA, SHETRAN_REAL_DATA, OUTPUT_TYPE, GET_OUTPUT_TYPE, &
      NO_SED, NO_CON, VERSION, ROOTDIR, SHETRAN_LAYER,                       &
      hdf5filename, planfile, checkfile
   USE VISUALISATION_INTERFACE_FAR_RIGHT, ONLY : G_C, G_L, G_I, S_PTR, G_PTR,                            &
      TIME_TO_RECORD,                                         &
      REGISTER_STATIC_VISUALISATION_METADATA,                 &
      REGISTER_DYNAMIC_VISUALISATION_METADATA,                &
      FOR_NEW_TIME, SAVE_ITEMS_WORTH,                         &
      SAVE_VISUALISATION_DATA_TO_DISK, VISUALISATION_TIDY_UP, &
      SEND_P

   IMPLICIT NONE

   INTEGER, DIMENSION(4), PARAMETER :: north_order = (/north, east, south, west/) !! HDF5/SHEGRAPH face order as SHETRAN face numbers.
   INTEGER, DIMENSION(4), PARAMETER :: normal_order = (/1,2,3,4/) !! Natural order for non-face extra dimensions.
   LOGICAL, PARAMETER    :: T=.TRUE.  !! Short true value for saved startup flags.
   LOGICAL, PARAMETER    :: F=.FALSE. !! Short false value for saved startup flags.

   REAL, PARAMETER :: zero=0.0 !! Real zero constant.

   PRIVATE
   PUBLIC :: RECORD_VISUALISATION_DATA, north_order

CONTAINS


!> Records visualisation data for the current simulation time.
!>
!> The first calls initialise metadata and static output; later calls populate
!> each scheduled dynamic item whose recording interval includes `time`.
   SUBROUTINE record_visualisation_data(time, text)
      REAL, INTENT(IN)                         :: time   !! Current simulation time.
      CHARACTER(*), INTENT(IN), OPTIONAL       :: text   !! Optional control text; `end` closes output resources.
      INTEGER                                  :: i      !! X/grid or catalogue loop index.
      INTEGER                                  :: j      !! HDF5/SHEGRAPH grid-row loop index.
      INTEGER                                  :: jj     !! SHETRAN grid-row index corresponding to `j`.
      INTEGER                                  :: k      !! Metadata-registration pass index.
      INTEGER                                  :: mn     !! Visualisation metadata item number.
      INTEGER                                  :: nn     !! Non-grid subunit-list index.
      INTEGER                                  :: su     !! SHETRAN subunit element number.
      INTEGER                                  :: ilow   !! Lower x/subunit index bound for the item.
      INTEGER                                  :: ihigh  !! Upper x/subunit index bound for the item.
      INTEGER                                  :: jlow   !! Lower HDF5/SHEGRAPH y-index bound.
      INTEGER                                  :: jhigh  !! Upper HDF5/SHEGRAPH y-index bound.
      INTEGER                                  :: klow   !! Lower SHEGRAPH layer index bound.
      INTEGER                                  :: khigh  !! Upper SHEGRAPH layer index bound.
      INTEGER                                  :: sz     !! Number of non-grid subunits in the item.
      INTEGER                                  :: ext    !! Number of extra-dimension entries.
      INTEGER                                  :: nsed   !! Sediment fraction number for the item.
      INTEGER                                  :: ncon   !! Contaminant number for the item.
      INTEGER                                  :: n      !! Implied-DO index for layer remapping.
      TYPE(C_PTR)                              :: first  !! Pointer to the first stored data slice.
      TYPE(C_PTR)                              :: latest !! Pointer to the latest stored data slice.
      LOGICAL                                  :: isgrid !! True when the item is laid out on the HDF5 grid.
      INTEGER, DIMENSION(4)                    :: ee     !! Extra-dimension values passed to SHETRAN accessors.
      CHARACTER(2)                             :: typ    !! Metadata type code plus static/dynamic suffix.
      CHARACTER(8)                             :: ext_dim !! Extra-dimension name for the item.
      CHARACTER(csz)                           :: name   !! Visualisation variable name.
      LOGICAL, SAVE                            :: one=T  !! First-call startup guard.
      LOGICAL, SAVE                            :: two=F  !! Second-call startup guard.
      TYPE(OUTPUT_TYPE), DIMENSION(:), POINTER :: oty    !! Static or dynamic catalogue subset.
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
         first = G_PTR(mn,'first')
         latest = G_PTR(mn,'latest')
         nsed   = G_I(mn,'nsed')
         ncon   = G_I(mn,'ncon')
         CALL FOR_NEW_TIME(typ, time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
         CALL S_PTR(mn,'first', first)
         CALL S_PTR(mn,'latest', latest)
         IF(.NOT.isgrid) THEN
            sz = G_I(mn,'sz')
            DO nn=1,sz
               su = G_I(mn,'su',nn)
               IF(su==0) CYCLE  !not a subunit _ so leave values at defaults
               CALL FILL_SELECT(name, typ, nn, 1, 1, su, klow, khigh, &
                  SHETRAN_LAYER((/(n,n=klow,khigh)/)), ee(1:ext), latest, nsed=nsed, ncon=ncon)
            ENDDO
         ELSE
            DO i=ilow,ihigh
               DO j=jlow,jhigh
                  jj = SHETRAN_J(j)  !SHETRAN grid is upside down
                  IF(.NOT.G_L(mn,'on', i, j)) CYCLE
                  su = SU_NUMBER(i,j)
                  IF(su==0) CYCLE  !not a subunit _ so leave values at defaults
                  CALL FILL_SELECT(name, typ, i, j, jj, su, klow, khigh, &
                     SHETRAN_LAYER((/(n,n=klow,khigh)/)), ee(1:ext), latest, nsed=nsed, ncon=ncon)
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

!> Dispatches a visualisation item to the appropriate filler for its data type.
   SUBROUTINE fill_select(name, typ, a, b, bb, su, klow, khigh, silay, ee, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name   !! Visualisation variable name.
      CHARACTER(*), INTENT(IN)          :: typ    !! Metadata type code plus static/dynamic suffix.
      INTEGER, INTENT(IN)               :: a      !! Output x index or non-grid subunit-list index.
      INTEGER, INTENT(IN)               :: b      !! Output HDF5/SHEGRAPH y index.
      INTEGER, INTENT(IN)               :: bb     !! SHETRAN y index.
      INTEGER, INTENT(IN)               :: su     !! SHETRAN subunit element number.
      INTEGER, INTENT(IN)               :: klow   !! Lower SHEGRAPH layer index.
      INTEGER, INTENT(IN)               :: khigh  !! Upper SHEGRAPH layer index.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay  !! SHETRAN layer numbers for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee     !! Extra-dimension values passed to SHETRAN accessors.
      TYPE(C_PTR), INTENT(IN)           :: latest !! Pointer to the latest stored data slice.
      INTEGER, INTENT(IN)               :: nsed   !! Sediment fraction number.
      INTEGER, INTENT(IN)               :: ncon   !! Contaminant number.
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


!> Fills real-valued bank data for an output item.
   SUBROUTINE fill_b(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name     !! Visualisation variable name.
      CHARACTER(*), INTENT(IN)          :: typ      !! Metadata type code plus static/dynamic suffix.
      INTEGER, INTENT(IN)               :: a        !! Output x index or non-grid subunit-list index.
      INTEGER, INTENT(IN)               :: b        !! Output HDF5/SHEGRAPH y index.
      INTEGER, INTENT(IN)               :: bb       !! SHETRAN y index.
      INTEGER, INTENT(IN)               :: su       !! SHETRAN grid-square subunit.
      INTEGER, INTENT(IN)               :: klow     !! Lower SHEGRAPH layer index.
      INTEGER, INTENT(IN)               :: khigh    !! Upper SHEGRAPH layer index.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay    !! SHETRAN layer numbers for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee       !! Extra-dimension values passed to SHETRAN accessors.
      TYPE(C_PTR), INTENT(IN)           :: latest   !! Pointer to the latest stored data slice.
      INTEGER, INTENT(IN)               :: nsed     !! Sediment fraction number.
      INTEGER, INTENT(IN)               :: ncon     !! Contaminant number.
      INTEGER                           :: d        !! HDF5/SHEGRAPH face slot, in `N,E,S,W` order.
      INTEGER                           :: e        !! Extra-dimension loop index.
      INTEGER                           :: banks(4) !! Bank element numbers around `su` in north-order slots.
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


!> Fills integer-valued bank data for an output item.
   SUBROUTINE fill_e(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name     !! Visualisation variable name.
      CHARACTER(*), INTENT(IN)          :: typ      !! Metadata type code plus static/dynamic suffix.
      INTEGER, INTENT(IN)               :: a        !! Output x index or non-grid subunit-list index.
      INTEGER, INTENT(IN)               :: b        !! Output HDF5/SHEGRAPH y index.
      INTEGER, INTENT(IN)               :: bb       !! SHETRAN y index.
      INTEGER, INTENT(IN)               :: su       !! SHETRAN grid-square subunit.
      INTEGER, INTENT(IN)               :: klow     !! Lower SHEGRAPH layer index.
      INTEGER, INTENT(IN)               :: khigh    !! Upper SHEGRAPH layer index.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay    !! SHETRAN layer numbers for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee       !! Extra-dimension values passed to SHETRAN accessors.
      TYPE(C_PTR), INTENT(IN)           :: latest   !! Pointer to the latest stored data slice.
      INTEGER, INTENT(IN)               :: nsed     !! Sediment fraction number.
      INTEGER, INTENT(IN)               :: ncon     !! Contaminant number.
      INTEGER                           :: d        !! HDF5/SHEGRAPH face slot, in `N,E,S,W` order.
      INTEGER                           :: e        !! Extra-dimension loop index.
      INTEGER                           :: banks(4) !! Bank element numbers around `su` in north-order slots.
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

!> Fills integer-valued river-link data for an output item.
   SUBROUTINE fill_f(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name      !! Visualisation variable name.
      CHARACTER(*), INTENT(IN)          :: typ       !! Metadata type code plus static/dynamic suffix.
      INTEGER, INTENT(IN)               :: a         !! Output x index or non-grid subunit-list index.
      INTEGER, INTENT(IN)               :: b         !! Output HDF5/SHEGRAPH y index.
      INTEGER, INTENT(IN)               :: bb        !! SHETRAN y index.
      INTEGER, INTENT(IN)               :: su        !! SHETRAN grid-square subunit.
      INTEGER, INTENT(IN)               :: klow      !! Lower SHEGRAPH layer index.
      INTEGER, INTENT(IN)               :: khigh     !! Upper SHEGRAPH layer index.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay     !! SHETRAN layer numbers for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee        !! Extra-dimension values passed to SHETRAN accessors.
      TYPE(C_PTR), INTENT(IN)           :: latest    !! Pointer to the latest stored data slice.
      INTEGER, INTENT(IN)               :: nsed      !! Sediment fraction number.
      INTEGER, INTENT(IN)               :: ncon      !! Contaminant number.
      INTEGER                           :: d         !! HDF5/SHEGRAPH face slot, in `N,E,S,W` order.
      INTEGER                           :: e         !! Extra-dimension loop index.
      INTEGER                           :: rivers(4) !! River-link element numbers around `su`.
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

!> Fills compound real data for a subunit and its adjacent banks and rivers.
   SUBROUTINE  fill_g(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name      !! Visualisation variable name.
      CHARACTER(*), INTENT(IN)          :: typ       !! Metadata type code plus static/dynamic suffix.
      INTEGER, INTENT(IN)               :: a         !! Output x index or non-grid subunit-list index.
      INTEGER, INTENT(IN)               :: b         !! Output HDF5/SHEGRAPH y index.
      INTEGER, INTENT(IN)               :: bb        !! SHETRAN y index.
      INTEGER, INTENT(IN)               :: su        !! SHETRAN grid-square subunit.
      INTEGER, INTENT(IN)               :: klow      !! Lower SHEGRAPH layer index.
      INTEGER, INTENT(IN)               :: khigh     !! Upper SHEGRAPH layer index.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay     !! SHETRAN layer numbers for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee        !! Extra-dimension values passed to SHETRAN accessors.
      TYPE(C_PTR), INTENT(IN)           :: latest    !! Pointer to the latest stored data slice.
      INTEGER, INTENT(IN)               :: nsed      !! Sediment fraction number.
      INTEGER, INTENT(IN)               :: ncon      !! Contaminant number.
      INTEGER                           :: d         !! HDF5/SHEGRAPH face slot, in `N,E,S,W` order.
      INTEGER                           :: e         !! Extra-dimension loop index.
      INTEGER                           :: banks(4)  !! Bank element numbers around `su`.
      INTEGER                           :: rivers(4) !! River-link element numbers around `su`.
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

!> Fills integer-valued grid-square data for an output item.
   SUBROUTINE  fill_i(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name   !! Visualisation variable name.
      CHARACTER(*), INTENT(IN)          :: typ    !! Metadata type code plus static/dynamic suffix.
      INTEGER, INTENT(IN)               :: a      !! Output x index or non-grid subunit-list index.
      INTEGER, INTENT(IN)               :: b      !! Output HDF5/SHEGRAPH y index.
      INTEGER, INTENT(IN)               :: bb     !! SHETRAN y index.
      INTEGER, INTENT(IN)               :: su     !! SHETRAN grid-square subunit.
      INTEGER, INTENT(IN)               :: klow   !! Lower SHEGRAPH layer index.
      INTEGER, INTENT(IN)               :: khigh  !! Upper SHEGRAPH layer index.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay  !! SHETRAN layer numbers for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee     !! Extra-dimension values passed to SHETRAN accessors.
      TYPE(C_PTR), INTENT(IN)           :: latest !! Pointer to the latest stored data slice.
      INTEGER, INTENT(IN)               :: nsed   !! Sediment fraction number.
      INTEGER, INTENT(IN)               :: ncon   !! Contaminant number.
      INTEGER                           :: d      !! Location slot passed through to `SAVE_ITEMS_WORTH`.
      INTEGER                           :: e      !! Extra-dimension loop index.
      INTEGER                           :: n      !! Retained local work index.
      DO e=1,SIZE(ee)
         CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
            SHETRAN_INTEGER_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
      ENDDO
   END SUBROUTINE fill_i

!> Fills real-valued river-link data for an output item.
   SUBROUTINE fill_L(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name      !! Visualisation variable name.
      CHARACTER(*), INTENT(IN)          :: typ       !! Metadata type code plus static/dynamic suffix.
      INTEGER, INTENT(IN)               :: a         !! Output x index or non-grid subunit-list index.
      INTEGER, INTENT(IN)               :: b         !! Output HDF5/SHEGRAPH y index.
      INTEGER, INTENT(IN)               :: bb        !! SHETRAN y index.
      INTEGER, INTENT(IN)               :: su        !! SHETRAN grid-square subunit.
      INTEGER, INTENT(IN)               :: klow      !! Lower SHEGRAPH layer index.
      INTEGER, INTENT(IN)               :: khigh     !! Upper SHEGRAPH layer index.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay     !! SHETRAN layer numbers for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee        !! Extra-dimension values passed to SHETRAN accessors.
      TYPE(C_PTR), INTENT(IN)           :: latest    !! Pointer to the latest stored data slice.
      INTEGER, INTENT(IN)               :: nsed      !! Sediment fraction number.
      INTEGER, INTENT(IN)               :: ncon      !! Contaminant number.
      INTEGER                           :: d         !! HDF5/SHEGRAPH face slot, in `N,E,S,W` order.
      INTEGER                           :: e         !! Extra-dimension loop index.
      INTEGER                           :: rivers(4) !! River-link element numbers around `su`.
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

!> Fills real-valued grid-square data for an output item.
   SUBROUTINE  fill_m(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name   !! Visualisation variable name.
      CHARACTER(*), INTENT(IN)          :: typ    !! Metadata type code plus static/dynamic suffix.
      INTEGER, INTENT(IN)               :: a      !! Output x index or non-grid subunit-list index.
      INTEGER, INTENT(IN)               :: b      !! Output HDF5/SHEGRAPH y index.
      INTEGER, INTENT(IN)               :: bb     !! SHETRAN y index.
      INTEGER, INTENT(IN)               :: su     !! SHETRAN grid-square subunit.
      INTEGER, INTENT(IN)               :: klow   !! Lower SHEGRAPH layer index.
      INTEGER, INTENT(IN)               :: khigh  !! Upper SHEGRAPH layer index.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay  !! SHETRAN layer numbers for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee     !! Extra-dimension values passed to SHETRAN accessors.
      TYPE(C_PTR), INTENT(IN)           :: latest !! Pointer to the latest stored data slice.
      INTEGER, INTENT(IN)               :: nsed   !! Sediment fraction number.
      INTEGER, INTENT(IN)               :: ncon   !! Contaminant number.
      INTEGER                           :: d      !! Location slot passed through to `SAVE_ITEMS_WORTH`.
      INTEGER                           :: e      !! Extra-dimension loop index.
      INTEGER                           :: n      !! Retained local work index.
      DO e=1,SIZE(ee)
         CALL SAVE_ITEMS_WORTH('m', typ, a, b, klow, khigh, e, d, &
            SHETRAN_REAL_DATA(name, su, ix=a, iy=bb, ilay=silay, ext=ee(e), nsed=nsed, ncon=ncon), latest)
      ENDDO
   END SUBROUTINE fill_m

!> Fills compound integer data for a subunit and its adjacent banks and rivers.
   SUBROUTINE  fill_n(name, a, b, bb, su, klow, khigh, silay, ee, typ, latest, nsed, ncon)
      CHARACTER(*), INTENT(IN)          :: name      !! Visualisation variable name.
      CHARACTER(*), INTENT(IN)          :: typ       !! Metadata type code plus static/dynamic suffix.
      INTEGER, INTENT(IN)               :: a         !! Output x index or non-grid subunit-list index.
      INTEGER, INTENT(IN)               :: b         !! Output HDF5/SHEGRAPH y index.
      INTEGER, INTENT(IN)               :: bb        !! SHETRAN y index.
      INTEGER, INTENT(IN)               :: su        !! SHETRAN grid-square subunit.
      INTEGER, INTENT(IN)               :: klow      !! Lower SHEGRAPH layer index.
      INTEGER, INTENT(IN)               :: khigh     !! Upper SHEGRAPH layer index.
      INTEGER, DIMENSION(:), INTENT(IN) :: silay     !! SHETRAN layer numbers for `klow:khigh`.
      INTEGER, DIMENSION(:), INTENT(IN) :: ee        !! Extra-dimension values passed to SHETRAN accessors.
      TYPE(C_PTR), INTENT(IN)           :: latest    !! Pointer to the latest stored data slice.
      INTEGER, INTENT(IN)               :: nsed      !! Sediment fraction number.
      INTEGER, INTENT(IN)               :: ncon      !! Contaminant number.
      INTEGER                           :: d         !! HDF5/SHEGRAPH face slot, in `N,E,S,W` order.
      INTEGER                           :: e         !! Extra-dimension loop index.
      INTEGER                           :: banks(4)  !! Bank element numbers around `su`.
      INTEGER                           :: rivers(4) !! River-link element numbers around `su`.
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

!> Sends setup metadata and geometry arrays to the far-right visualisation layer.
   SUBROUTINE send_pass(jj)
      INTEGER, INTENT(IN)                  :: jj !! Setup pass selector: 1 file metadata, 2 geometry metadata.
      INTEGER                              :: i  !! X/grid or element loop index.
      INTEGER                              :: j  !! Y/grid or face loop index.
      INTEGER                              :: nx !! Number of HDF5/SHEGRAPH grid columns.
      INTEGER                              :: ny !! Number of HDF5/SHEGRAPH grid rows.
      INTEGER                              :: total_no_elements !! Number of SHETRAN elements.
      INTEGER, DIMENSION(:), ALLOCATABLE   :: iel !! Element index vector `1:NO_EL()`.
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: dum !! Temporary integer grid/table sent through `SEND_P`.

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

!> Returns the subunit number at HDF5 visualisation grid coordinates.
   ELEMENTAL INTEGER FUNCTION su_number(i,j) RESULT(r)
      INTEGER, INTENT(IN) :: i !! HDF5/SHEGRAPH x index.
      INTEGER, INTENT(IN) :: j !! HDF5/SHEGRAPH y index.
      r = ELEMENT(i,SHETRAN_J(j))  !SHETRAN grid is upside down
   END FUNCTION su_number

!> Converts an HDF5/SHEGRAPH y-index to the SHETRAN y-index.
!>
!> \[
!> r = GRID\_NY() - sgv2j + 1
!> \]
   ELEMENTAL INTEGER FUNCTION shetran_j(sgv2j) RESULT(r) !grid y coordinate
      INTEGER, INTENT(IN) :: sgv2j !! HDF5/SHEGRAPH y index.
      r = GRID_NY() - sgv2j + 1
   END FUNCTION shetran_j

END MODULE visualisation_interface_right
