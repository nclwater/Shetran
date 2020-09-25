!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_map
!DEC$ REAL:4
USE VISUALISATION_PASS,     ONLY : BANK_NO, SU_NUMBER, RIVER_NO, north, east, south, west, IS_LINK
USE VISUALISATION_METADATA, ONLY : G_L=>GET_METADATA_L

IMPLICIT NONE

INTEGER, PARAMETER :: mmax=255, i_background=0, i_river=mmax-1
REAL, PARAMETER    :: no_data=-1.0, background=0.0, river =HUGE(1.0)


PRIVATE
PUBLIC :: GET_REAL_IMAGE_INDEX, GET_MAGNIFIED_SU_ARR

CONTAINS


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE FUNCTION get_real_image_index(sz, dat, mag, mn) RESULT(r)
INTEGER, DIMENSION(:,:), POINTER     :: r
INTEGER, INTENT(IN)                  :: mag, mn  !magnification
INTEGER, DIMENSION(:),INTENT(IN)     :: sz
REAL, DIMENSION(:,:,:), INTENT(IN)   :: dat
REAL, DIMENSION(:,:), POINTER        :: rreal
REAL                                 :: minr, maxr

rreal => GET_MAGNIFIED_REAL(sz, dat, mag, mn, mark_river=.TRUE.)
minr  =  MINVAL(rreal, MASK=(rreal/=river .AND. rreal/=background))
maxr  =  MAXVAL(rreal, MASK=(rreal/=river .AND. rreal/=background))

ALLOCATE(r(mag*sz(1),mag*sz(2)))
WHERE(rreal==river)
    r = i_river
ELSEWHERE(rreal==background)
    r = i_background
ELSEWHERE
    r = 15 + (mmax-17) * (rreal-minr)/(maxr-minr)  !scaling
ENDWHERE

DEALLOCATE(rreal)
END FUNCTION get_real_image_index

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE FUNCTION get_magnified_real(sz, dat, mag, mn, mark_river) RESULT(r)
INTEGER, INTENT(IN)                 :: mag, mn  !magnification
INTEGER                             :: i, j, im, jm, ilow, ihigh, jlow, jhigh, SU
INTEGER, DIMENSION(:),INTENT(IN)    :: sz
REAL, DIMENSION(:,:,:), INTENT(IN)  :: dat
REAL, DIMENSION(:,:), POINTER       :: r
LOGICAL, INTENT(IN)                 :: mark_river

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

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE FUNCTION get_dat_r(d9, su, mag, mark_river)RESULT(r)
INTEGER, INTENT(IN)            :: su, mag
REAL, DIMENSION(mag,mag)       :: r
INTEGER                        :: j, b
REAL, DIMENSION(9), INTENT(IN) :: d9
REAL                           :: dum
LOGICAL, INTENT(IN)            :: mark_river
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


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE FUNCTION get_is_link_magnified(sz, mag, mn) RESULT(r)
INTEGER, INTENT(IN)                 :: mag, mn  !magnification
INTEGER                             :: i
INTEGER, DIMENSION(:),INTENT(IN)    :: sz
INTEGER, DIMENSION(:,:), POINTER    :: su
LOGICAL, DIMENSION(:,:), POINTER    :: r
su => GET_MAGNIFIED_SU_ARR(sz, mag, mn)
ALLOCATE(r(mag*sz(1),mag*sz(2)))
DO i=1,mag*sz(1)
    r(i,:) = IS_LINK(su(i,:))
ENDDO
DEALLOCATE(su)
END FUNCTION get_is_link_magnified


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE FUNCTION get_magnified_su_arr(sz, mag, mn) RESULT(r)
INTEGER, INTENT(IN)                 :: mag, mn  !magnification
INTEGER, DIMENSION(:),INTENT(IN)    :: sz
INTEGER, DIMENSION(:,:), POINTER    :: r
INTEGER, DIMENSION(mag,mag)         :: el
INTEGER                             :: i, j, im, jm, ilow, ihigh, jlow, jhigh, su

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


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE FUNCTION get_el(su, mag)RESULT(r)
INTEGER, INTENT(IN)         :: su, mag
INTEGER, DIMENSION(mag,mag) :: r
INTEGER                     :: j
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