!> summary: Mapping utilities for visualisation rasters.
!>
!> This module converts SHETRAN subunit, bank, and link values onto magnified
!> image grids for visualisation output. The routines expand each active model
!> cell into a `mag` by `mag` block, reserving edge strips for river and bank
!> elements so that raster outputs can distinguish cell interiors from channel
!> faces.
!>
!> Magnified cell layout:
!>
!> | Source | Component in `d9` | Block position |
!> |:-------|:------------------|:---------------|
!> | Subunit interior | 1 | Main block interior. |
!> | North bank | 2 | Rows `3:mag-2`, columns `3:4`. |
!> | East bank | 3 | Rows `mag-3:mag-2`, columns `3:mag-2`. |
!> | South bank | 4 | Rows `3:mag-2`, columns `mag-3:mag-2`. |
!> | West bank | 5 | Rows `3:4`, columns `3:mag-2`. |
!> | North river/link | 6 | Rows `3:mag-2`, columns `1:2`. |
!> | East river/link | 7 | Rows `mag-1:mag`, columns `3:mag-2`. |
!> | South river/link | 8 | Rows `3:mag-2`, columns `mag-1:mag`. |
!> | West river/link | 9 | Rows `1:2`, columns `3:mag-2`. |
!>
!> The hard-coded strips assume `mag >= 6` if banks, rivers, and cell interior
!> are to remain visually distinct.
MODULE visualisation_map

USE VISUALISATION_PASS,     ONLY : BANK_NO, SU_NUMBER, RIVER_NO, north, east, south, west, IS_LINK
USE VISUALISATION_METADATA, ONLY : G_L=>GET_METADATA_L

IMPLICIT NONE

INTEGER, PARAMETER :: mmax=255        !! Maximum image palette index.
INTEGER, PARAMETER :: i_background=0  !! Palette index used for inactive background pixels.
INTEGER, PARAMETER :: i_river=mmax-1  !! Palette index used for river/link pixels.
REAL, PARAMETER    :: no_data=-1.0    !! Missing component sentinel in 9-part cell data.
REAL, PARAMETER    :: background=0.0  !! Background value before palette indexing.
REAL, PARAMETER    :: river=HUGE(1.0) !! River/link sentinel before palette indexing.


PRIVATE
PUBLIC :: GET_REAL_IMAGE_INDEX, GET_MAGNIFIED_SU_ARR

CONTAINS


!> Converts a real visualisation field to an indexed image grid.
!>
!> Active model values are magnified, river and background cells are assigned
!> fixed palette indices, and the remaining values are linearly scaled over the
!> available colour range.
!>
!> Entry assumptions: `sz` has at least two entries, `mag >= 6` for the standard
!> block layout, and the magnified real grid contains at least one non-background
!> and non-river value with `maxr > minr`.
PURE FUNCTION get_real_image_index(sz, dat, mag, mn) RESULT(r)
INTEGER, DIMENSION(:,:), ALLOCATABLE :: r     !! Indexed image grid.
INTEGER, INTENT(IN)                  :: mag   !! Magnification factor for each model cell.
INTEGER, INTENT(IN)                  :: mn    !! Visualisation metadata item number used for active-cell masking.
INTEGER, DIMENSION(:),INTENT(IN)     :: sz    !! Two-element source grid size.
REAL, DIMENSION(:,:,:), INTENT(IN)   :: dat   !! Real values indexed by component and source grid location.
REAL, DIMENSION(:,:), ALLOCATABLE    :: rreal !! Magnified real-valued grid before palette indexing.
REAL                                 :: minr  !! Minimum active non-river value.
REAL                                 :: maxr  !! Maximum active non-river value.
INTEGER                              :: i     !! Output x-index loop counter.
INTEGER                              :: j     !! Output y-index loop counter.

rreal = GET_MAGNIFIED_REAL(sz, dat, mag, mn, mark_river=.TRUE.)
minr  =  MINVAL(rreal, MASK=(rreal/=river .AND. rreal/=background))
maxr  =  MAXVAL(rreal, MASK=(rreal/=river .AND. rreal/=background))

ALLOCATE(r(mag*sz(1),mag*sz(2)))
DO j = 1, mag*sz(2)
    DO i = 1, mag*sz(1)
        IF (rreal(i,j) == river) THEN
            r(i,j) = i_river
        ELSE IF (rreal(i,j) == background) THEN
            r(i,j) = i_background
        ELSE
            r(i,j) = 15 + (mmax-17) * (rreal(i,j)-minr)/(maxr-minr)  !scaling
        END IF
    END DO
END DO

DEALLOCATE(rreal)
END FUNCTION get_real_image_index

!> Magnifies a real-valued visualisation field onto an output image grid.
PURE FUNCTION get_magnified_real(sz, dat, mag, mn, mark_river) RESULT(r)
INTEGER, INTENT(IN)                 :: mag   !! Magnification factor for each model cell.
INTEGER, INTENT(IN)                 :: mn    !! Visualisation metadata item number used for active-cell masking.
INTEGER, DIMENSION(:),INTENT(IN)    :: sz    !! Two-element source grid size.
REAL, DIMENSION(:,:,:), INTENT(IN)  :: dat   !! Real values indexed by component and source grid location.
LOGICAL, INTENT(IN)                 :: mark_river !! If true, river strips are marked with the river sentinel value.
REAL, DIMENSION(:,:), ALLOCATABLE   :: r     !! Magnified real-valued grid.
INTEGER                             :: i     !! Source x-index loop counter.
INTEGER                             :: j     !! Source y-index loop counter.
INTEGER                             :: im    !! Magnified x-offset for the current source cell.
INTEGER                             :: jm    !! Magnified y-offset for the current source cell.
INTEGER                             :: ilow  !! Lower source x-index.
INTEGER                             :: ihigh !! Upper source x-index.
INTEGER                             :: jlow  !! Lower source y-index.
INTEGER                             :: jhigh !! Upper source y-index.
INTEGER                             :: su    !! SHETRAN subunit number for the source cell.

ALLOCATE(r(mag*sz(1),mag*sz(2)))

ilow  = 1
ihigh = sz(1)
jlow  = 1 
jhigh = sz(2)
im    = -mag
r     = 0
DO i=ilow,ihigh
    im = im + mag
    jm = -mag
    DO j=jlow,jhigh
        jm = jm + mag
        IF(.NOT.G_L(mn,'on', i, j)) CYCLE
        su = SU_NUMBER(i,j)
        IF(su==0) CYCLE  !not a subunit _ so leave values at defaults
        r(im+1:im+mag,jm+1:jm+mag) = GET_DAT_R(dat(:,i,j), su, mag, mark_river)
    ENDDO
ENDDO

END FUNCTION get_magnified_real

!> Builds one magnified cell block from subunit, bank, and river component values.
!>
!> `d9=no_data` suppresses the corresponding bank or river strip. When
!> `mark_river` is true, river/link strips use the `river` sentinel instead of
!> the source value so that [[get_real_image_index]] can assign a fixed palette
!> index.
PURE FUNCTION get_dat_r(d9, su, mag, mark_river)RESULT(r)
INTEGER, INTENT(IN)            :: su !! Subunit number for the model cell.
INTEGER, INTENT(IN)            :: mag !! Magnification factor for each model cell.
REAL, DIMENSION(mag,mag)       :: r  !! Magnified cell block.
INTEGER                        :: b  !! `d9` component index.
REAL, DIMENSION(9), INTENT(IN) :: d9 !! Cell, bank, and river values for the magnified block.
REAL                           :: dum !! Value written to river/link strips.
LOGICAL, INTENT(IN)            :: mark_river !! If true, river strips are marked with the river sentinel value.
r        = d9(1)
r(:,1)   = 0
r(:,mag) = 0
r(1, :)  = 0
r(mag,:) = 0
IF(su==0) RETURN
DO b=2,9
    IF(d9(b)/=no_data) THEN
        IF(mark_river) THEN
            dum=river
        ELSE
            dum = d9(b)
        ENDIF
        SELECT CASE(b)
        CASE(2) ; r(3:mag-2    ,3:4)         = d9(b)
        CASE(3) ; r(mag-3:mag-2,3:mag-2)     = d9(b)
        CASE(4) ; r(3:mag-2    ,mag-3:mag-2) = d9(b)
        CASE(5) ; r(3:4        ,3:mag-2)     = d9(b)
        CASE(6) ; r(3:mag-2    ,1:2)         = dum
        CASE(7) ; r(mag-1:mag  ,3:mag-2)     = dum
        CASE(8) ; r(3:mag-2    ,mag-1:mag)   = dum
        CASE(9) ; r(1:2        ,3:mag-2)     = dum
        END SELECT
    ENDIF
ENDDO

END FUNCTION get_dat_r


!> Builds a magnified logical mask showing river-link cells.
PURE FUNCTION get_is_link_magnified(sz, mag, mn) RESULT(r)
INTEGER, INTENT(IN)                  :: mag !! Magnification factor for each model cell.
INTEGER, INTENT(IN)                  :: mn  !! Visualisation metadata item number used for active-cell masking.
INTEGER, DIMENSION(:),INTENT(IN)     :: sz  !! Two-element source grid size.
LOGICAL, DIMENSION(:,:), ALLOCATABLE :: r   !! Magnified logical mask; true for river-link pixels.
INTEGER                              :: i   !! Magnified x-index loop counter.
INTEGER, DIMENSION(:,:), ALLOCATABLE :: su  !! Magnified subunit-number grid.
su = GET_MAGNIFIED_SU_ARR(sz, mag, mn)
ALLOCATE(r(mag*sz(1),mag*sz(2)))
DO i=1,mag*sz(1)
    r(i,:) = IS_LINK(su(i,:))
ENDDO
DEALLOCATE(su)
END FUNCTION get_is_link_magnified


!> Magnifies the subunit-number array onto an output image grid.
PURE FUNCTION get_magnified_su_arr(sz, mag, mn) RESULT(r)
INTEGER, INTENT(IN)                  :: mag   !! Magnification factor for each model cell.
INTEGER, INTENT(IN)                  :: mn    !! Visualisation metadata item number used for active-cell masking.
INTEGER, DIMENSION(:),INTENT(IN)     :: sz    !! Two-element source grid size.
INTEGER, DIMENSION(:,:), ALLOCATABLE :: r     !! Magnified subunit/element-number grid.
INTEGER                              :: i     !! Source x-index loop counter.
INTEGER                              :: j     !! Source y-index loop counter.
INTEGER                              :: im    !! Magnified x-offset for the current source cell.
INTEGER                              :: jm    !! Magnified y-offset for the current source cell.
INTEGER                              :: ilow  !! Lower source x-index.
INTEGER                              :: ihigh !! Upper source x-index.
INTEGER                              :: jlow  !! Lower source y-index.
INTEGER                              :: jhigh !! Upper source y-index.
INTEGER                              :: su    !! SHETRAN subunit number for the source cell.

ALLOCATE(r(mag*sz(1),mag*sz(2)))
ilow  = 1
ihigh = sz(1)
jlow  = 1 
jhigh = sz(2)
im    = -mag
r     = 0
DO i=ilow,ihigh
    im = im + mag
    jm = -mag
    DO j=jlow,jhigh
        jm = jm + mag
        IF(.NOT.G_L(mn,'on', i, j)) CYCLE
        su = SU_NUMBER(i,j)
        IF(su==0) CYCLE  !not a subunit _ so leave values at defaults
        r(im+1:im+mag,jm+1:jm+mag) = GET_EL(su, mag)
    ENDDO
ENDDO
END FUNCTION get_magnified_su_arr


!> Returns the magnified element-number block for one subunit.
!>
!> The cell interior is initialised to `su`; outer pixel borders are background
!> zero; bank and river/link strips are overwritten with the adjacent element
!> number when present.
PURE FUNCTION get_el(su, mag)RESULT(r)
INTEGER, INTENT(IN)         :: su !! Subunit number for the model cell.
INTEGER, INTENT(IN)         :: mag !! Magnification factor for each model cell.
INTEGER, DIMENSION(mag,mag) :: r  !! Magnified element-number block.
INTEGER                     :: j  !! Adjacent bank or river-link element number.
r = su
!cell orders
r(:,1)   = 0
r(:,mag) = 0
r(1, :)  = 0
r(mag,:) = 0
IF(su==0) RETURN
j = RIVER_NO(su,north) ; IF(j>0) r(3:mag-2    ,1:2)         = j
j = BANK_NO(su,north)  ; IF(j>0) r(3:mag-2    ,3:4)         = j
j = BANK_NO(su,south)  ; IF(j>0) r(3:mag-2    ,mag-3:mag-2) = j
j = RIVER_NO(su,south) ; IF(j>0) r(3:mag-2    ,mag-1:mag)   = j
j = RIVER_NO(su,west)  ; IF(j>0) r(1:2        ,3:mag-2)     = j
j = BANK_NO(su,west)   ; IF(j>0) r(3:4        ,3:mag-2)     = j
j = BANK_NO(su,east)   ; IF(j>0) r(mag-3:mag-2,3:mag-2)     = j
j = RIVER_NO(su,east)  ; IF(j>0) r(mag-1:mag  ,3:mag-2)     = j
END FUNCTION get_el
END MODULE visualisation_map
