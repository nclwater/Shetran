!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_structure
!DEC$ REAL:4
!JE for SHEGRAPH Version 2.0 Created July 2004
IMPLICIT NONE

INTEGER, PARAMETER :: iundef      = -1,      & !undefined
                      i_not_exist = iundef, &
                      defi4(4)    = (/iundef, iundef, iundef, iundef/), &
                      csz         = 70         !name length
REAL, PARAMETER    :: zero=0.0, half=0.5,                           &
                      rundef=-1.0, & !undefined
                      r_not_exist = rundef, &
                      defr4(4)       = (/r_not_exist, r_not_exist, r_not_exist, r_not_exist/)
LOGICAL, PARAMETER :: t=.TRUE., f=.FALSE.

INTEGER, PARAMETER :: no_types=8

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE aord
INTEGER, POINTER :: a
END TYPE aord


!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE CYPHER
CHARACTER      :: nemonic
CHARACTER(CSZ) :: typ
END TYPE CYPHER
TYPE(CYPHER), PARAMETER :: cyph(no_types)=        &
      (/cypher('B', 'real_banks'),                &
        cypher('E', 'integer_banks'),             &
        cypher('F', 'integer_rivers'),            &
        cypher('G', 'real_middle_and_edges'),     &
        cypher('I', 'integer_middle'),            &
        cypher('L', 'real_rivers'),               &
        cypher('M', 'real_middle'),               &
        cypher('N', 'integer_middle_and_edges')/)

!EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE integer_edges !for middle of cell and edges
INTEGER :: e(4) = iundef    !edge N, E, S, W
END TYPE integer_edges
TYPE(INTEGER_EDGES), PARAMETER :: &
        default_integer_edges = INTEGER_EDGES(defi4), &
        dfie                  = default_integer_edges
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE real_edges !for middle of cell and edges
REAL :: e(4) = rundef  !edges N, E, S, W
END TYPE real_edges
TYPE(REAL_EDGES), PARAMETER :: &
        default_real_edges = REAL_EDGES(defr4), &
        dfre               = default_real_edges
!MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE integer_middle !for middle of cell
INTEGER :: m = rundef  !middle
END TYPE integer_middle
TYPE(INTEGER_MIDDLE), PARAMETER :: &
        default_integer_middle = INTEGER_MIDDLE(r_not_exist), &
        dfim                   = default_integer_middle
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE real_middle !for middle of cell
REAL :: m    = rundef  !middle
END TYPE real_middle
TYPE(REAL_MIDDLE), PARAMETER :: &
        default_real_middle = REAL_MIDDLE(r_not_exist), &
        dfrm                = default_real_middle
!MIDDLE AND EDGES MIDDLE AND EDGES MIDDLE AND EDGES MIDDLE AND EDGES MIDDLE AND EDGES 
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE integer_middle_and_edges !for middle of cell and edges
PRIVATE
INTEGER :: m    = iundef, &  !middle
           b(4) = iundef, &  !bank N, E, S, W
           r(4) = iundef     !river N, E, S, W
END TYPE integer_middle_and_edges
TYPE(INTEGER_MIDDLE_AND_EDGES), PARAMETER :: &
        default_integer_middle_and_edges = INTEGER_MIDDLE_AND_EDGES(i_not_exist, defi4, defi4), &
        dfime                            = default_integer_middle_and_edges
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE real_middle_and_edges !for middle of cell and edges
sequence
REAL :: m    = rundef, &  !middle
        b(4) = rundef, &  !bank N, E, S, W
        r(4) = rundef     !river N, E, S, W
END TYPE real_middle_and_edges
TYPE(REAL_MIDDLE_AND_EDGES), PARAMETER :: &
        default_real_middle_and_edges = REAL_MIDDLE_AND_EDGES(r_not_exist, defr4, defr4), &
        dfrme                         = default_real_middle_and_edges
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE integer_radial !for middle of cell and edges
INTEGER :: m(4) = iundef, &  !middle
           b(4) = iundef, &  !bank N, E, S, W
           r(4) = iundef     !river N, E, S, W
END TYPE integer_radial
TYPE(INTEGER_RADIAL), PARAMETER :: &
        default_integer_radial = INTEGER_RADIAL(i_not_exist, defi4, defi4), &
        dfir                  = default_integer_radial
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE real_radial !for middle of cell and edges
REAL :: m(4) = rundef, &  !middle
        b(4) = rundef, &  !bank N, E, S, W
        r(4) = rundef     !river N, E, S, W
END TYPE real_radial
TYPE(REAL_RADIAL), PARAMETER :: &
        default_real_radial = REAL_radial(r_not_exist, defr4, defr4), &
        dfrr                = default_real_radial


!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE BS
PRIVATE
REAL                                          :: time=zero
TYPE(REAL_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(BS), POINTER                             :: previous=>NULL(), next=>NULL()
END TYPE BS
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE ES
PRIVATE
REAL                                             :: time=zero
TYPE(INTEGER_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(ES), POINTER                                :: previous=>NULL(), next=>NULL()
END TYPE ES
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE FS
PRIVATE
REAL                                             :: time=zero
TYPE(INTEGER_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(FS), POINTER                                :: previous=>NULL(), next=>NULL()
END TYPE FS
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE GS
PRIVATE
sequence
REAL                                                     :: time=zero
TYPE(REAL_MIDDLE_AND_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(GS), POINTER                                        :: previous=>NULL(), next=>NULL()
END TYPE GS
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE IS
PRIVATE
REAL                                              :: time=zero
TYPE(INTEGER_MIDDLE), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(IS), POINTER                                 :: previous=>NULL(), next=>NULL()
END TYPE IS
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE LS
PRIVATE
REAL                                          :: time=zero
TYPE(REAL_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(LS), POINTER                             :: previous=>NULL(), next=>NULL()
END TYPE LS
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE MS
PRIVATE
REAL                                           :: time=zero
TYPE(MS), POINTER                              :: previous=>NULL(), next=>NULL()
TYPE(REAL_MIDDLE), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
END TYPE MS
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TYPE NS
PRIVATE
REAL                                                       :: time=zero
TYPE(INTEGER_MIDDLE_AND_EDGES), DIMENSION(:,:,:,:), POINTER :: s=>NULL()  !i,j,k,d
TYPE(NS), POINTER                                           :: previous=>NULL(), next=>NULL()
END TYPE NS


INTEGER(INT_PTR_KIND())       :: llistend
TYPE(BS), TARGET :: llistend_b ; POINTER(llistend, llistend_b)
TYPE(ES), TARGET :: llistend_e ; POINTER(llistend, llistend_e)
TYPE(FS), TARGET :: llistend_f ; POINTER(llistend, llistend_f)
TYPE(GS), TARGET :: llistend_g ; POINTER(llistend, llistend_g)
TYPE(IS), TARGET :: llistend_i ; POINTER(llistend, llistend_i)
TYPE(LS), TARGET :: llistend_l ; POINTER(llistend, llistend_l)
TYPE(MS), TARGET :: llistend_m ; POINTER(llistend, llistend_m)
TYPE(NS), TARGET :: llistend_n ; POINTER(llistend, llistend_n)

INTERFACE SAVE_ITEMS_WORTH ; MODULE PROCEDURE SAVE_ITEMS_WORTH_I, SAVE_ITEMS_WORTH_R ; ENDINTERFACE

PRIVATE
PUBLIC :: FOR_NEW_TIME, SAVE_ITEMS_WORTH, TIME_COUNT, MBR_COUNT, GET_MBR, GET_HDF5_I, GET_HDF5_R, &
          GET_HDF5_TIME, csz
         

CONTAINS

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
REAL FUNCTION get_hdf5_time(typ, first) RESULT(r)
INTEGER(INT_PTR_KIND()), INTENT(IN)      :: first
CHARACTER(*), INTENT(IN) :: typ
TYPE(BS), POINTER        :: pb
TYPE(ES), POINTER        :: pe
TYPE(FS), POINTER        :: pf
TYPE(GS), POINTER        :: pg
TYPE(IS), POINTER        :: pi
TYPE(LS), POINTER        :: pl
TYPE(MS), POINTER        :: pm
TYPE(NS), POINTER        :: pn
llistend = first
SELECT CASE(typ)
CASE('BS') ; pb => llistend_b ; r = pb%time
CASE('ES') ; pe => llistend_e ; r = pe%time
CASE('FS') ; pf => llistend_f ; r = pf%time
CASE('GS') ; pg => llistend_g ; r = pg%time
CASE('IS') ; pi => llistend_i ; r = pf%time
CASE('LS') ; pl => llistend_l ; r = pl%time
CASE('MS') ; pm => llistend_m ; r = pm%time
CASE('NS') ; pn => llistend_n ; r = pn%time
END SELECT
END FUNCTION get_hdf5_time


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION get_hdf5_i(typ, sz, szo, first, ilow, jlow, klow) RESULT(r)
INTEGER, INTENT(IN)                                     :: ilow, jlow, klow
INTEGER(INT_PTR_KIND()), INTENT(INOUT)                                  :: first
INTEGER, DIMENSION(6), INTENT(IN)                       :: sz, szo
INTEGER, DIMENSION(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)) :: r
CHARACTER(*), INTENT(IN)                                :: typ
CALL GET_HDF5(typ, sz, szo, first, ilow, jlow, klow, rint=r)
END FUNCTION get_hdf5_i

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION get_hdf5_r(typ, sz, szo, first, ilow, jlow, klow) RESULT(r)
INTEGER, INTENT(IN)                                     :: ilow, jlow, klow
INTEGER(INT_PTR_KIND()), INTENT(INOUT)                                  :: first
INTEGER, DIMENSION(6), INTENT(IN)                       :: sz, szo
REAL, DIMENSION(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6))    :: r
CHARACTER(*), INTENT(IN)                                :: typ
CALL GET_HDF5(typ, sz, szo, first, ilow, jlow, klow, rreal=r)
END FUNCTION get_hdf5_r

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE get_hdf5(typ, sz, szo, first, ilow, jlow, klow, rint, rreal)
INTEGER, INTENT(IN)                     :: ilow, jlow, klow
INTEGER(INT_PTR_KIND()), INTENT(INOUT)               :: first
!INTEGER(INT_PTR_KIND())                 :: dum1, dum2, dum3
INTEGER, DIMENSION(6), INTENT(IN)       :: sz, szo
INTEGER                                 :: szii, szjj, szkk, szcc, szee, sztt, ii, jj, kk
INTEGER, TARGET                         :: dii, djj, dkk, cc, ee, tt
TYPE(AORD), DIMENSION(:), POINTER, SAVE :: d
INTEGER, DIMENSION(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)), INTENT(OUT), OPTIONAL :: rint
REAL,    DIMENSION(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)), INTENT(OUT), OPTIONAL :: rreal
CHARACTER(*), INTENT(IN) :: typ
TYPE(BS), POINTER        :: pb
TYPE(ES), POINTER        :: pe
TYPE(FS), POINTER        :: pf
TYPE(GS), POINTER        :: pg, dumpg
TYPE(IS), POINTER        :: pi
TYPE(LS), POINTER        :: pl
TYPE(MS), POINTER        :: pm
TYPE(NS), POINTER        :: pn

LOGICAL, SAVE            :: initial=T
 IF(initial) THEN
    initial = F
    ALLOCATE(d(6))
ENDIF

szii = sz(szo(1)) ; d(szo(1))%a=>dii
szjj = sz(szo(2)) ; d(szo(2))%a=>djj
szkk = sz(szo(3)) ; d(szo(3))%a=>dkk
szcc = sz(szo(4)) ; d(szo(4))%a=>cc
szee = sz(szo(5)) ; d(szo(5))%a=>ee
sztt = sz(szo(6)) ; d(szo(6))%a=>tt
IF(PRESENT(rint)) THEN ; rint = 0 ; ELSEIF(PRESENT(rreal)) THEN ; rreal=zero ; ENDIF
tt       = 1
llistend = first
SELECT CASE(TYP)
CASE('BS')  !real banks
    pb => llistend_b
    CALL MAIN_LOOP('BS')
    IF(ASSOCIATED(pb%next)) THEN ; first = LOC(pb%next) ; ELSE ; first=0 ; ENDIF ; CALL DEALL_PB(pb)
CASE('ES')  !integer banks
    pe => llistend_e
    CALL MAIN_LOOP('ES')
    IF(ASSOCIATED(pe%next)) THEN ; first = LOC(pe%next) ;  ELSE ; first=0 ; ENDIF ; CALL DEALL_PE(pe)
CASE('FS')  !integer rivers
    pf => llistend_f
    CALL MAIN_LOOP('FS')
    IF(ASSOCIATED(pf%next)) THEN ; first = LOC(pf%next) ;  ELSE ; first=0 ; ENDIF ; CALL DEALL_PF(pf)
CASE('GS')  !real middle and edges
    !!!!!!!!!!!!IF(.NOT.ASSOCIATED(llistend_g%s)) then
    !!!!!!!!!!!!    print*, 'failed LLLLLLLLLLLLLLL'
    !!!!!!!!!!!!    stop
    !!!!!!!!!!!!else
    !!!!!!!!!!!!    print*, size(llistend_g%s,dim=1)
    !!!!!!!!!!!!endif
    !!!!!!!!!!!!fred => llistend_g
    pg => llistend_g
    !    dumpg=>pg
    !dum1 = loc(pg)
    !dum2 = loc(llistend_g)
    !dum3 = loc(dumpg)
    !print*, dum1, dum2, dum3
    !!!!!!!!!!!!IF(.NOT.ASSOCIATED(pg%s)) then
    !!!!!!!!!!!!    print*, 'failed HHHHHHHHHHHHHHH'
    !!!!!!!!!!!!    stop
    !!!!!!!!!!!!endif
    CALL MAIN_LOOP('GS')
    IF(ASSOCIATED(pg%next)) THEN ; first = LOC(pg%next) ;  ELSE ; first=0 ; ENDIF ; CALL DEALL_PG(pg)
CASE('IS')  !integer middle
    pi => llistend_i
    CALL MAIN_LOOP('IS')
    IF(ASSOCIATED(pi%next)) THEN ; first = LOC(pi%next) ;  ELSE ; first=0 ; ENDIF ; CALL DEALL_PI(pi)
CASE('LS')  !real banks
    pl => llistend_l
    CALL MAIN_LOOP('LS')
    IF(ASSOCIATED(pl%next)) THEN ; first = LOC(pl%next) ;  ELSE ; first=0 ; ENDIF ; CALL DEALL_PL(pl)
CASE('MS')  !real middle
    pm => llistend_m
    CALL MAIN_LOOP('MS')
    IF(ASSOCIATED(pm%next)) THEN ; first = LOC(pm%next) ;  ELSE ; first=0 ; ENDIF ; CALL DEALL_PM(pm)
CASE('NS')  !integer middle and edges
    pn => llistend_n
    CALL MAIN_LOOP('NS')
    IF(ASSOCIATED(pn%next)) THEN ; first = LOC(pn%next) ;  ELSE ; first=0 ; ENDIF ; CALL DEALL_PN(pn)
END SELECT


CONTAINS

    !cscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscsc
    SUBROUTINE main_loop(text)  !there is a similar routine in get_hdf5_r
    INTEGER                  :: idum
    REAL                     :: rdum
    CHARACTER(*), INTENT(IN) :: text
    DO dii=1,szii ; ii=ilow+dii-1
        DO djj=1,szjj ; jj=jlow+djj-1
            DO dkk=1,szkk ; kk=klow+dkk-1
                DO ee=1,szee
                    DO cc=1,szcc
                        IF(PRESENT(rint)) THEN
                            SELECT CASE(text)
                            CASE('ES') ; idum = pe%s(ii, jj, kk, ee)%e(cc)
                            CASE('FS') ; idum = pf%s(ii, jj, kk, ee)%e(cc)
                            CASE('IS') ; idum = pi%s(ii, jj, kk, ee)%m
                            CASE('NS') ; idum = FNS()
                            END SELECT
                            rint(d(1)%a,d(2)%a,d(3)%a,d(4)%a,d(5)%a,d(6)%a) = idum
                        ELSEIF(PRESENT(rreal)) THEN
                            SELECT CASE(text)
                            CASE('BS') ; rdum = pb%s(ii, jj, kk, ee)%e(cc)
                            CASE('GS') ; rdum = FGS()
                            CASE('LS') ; rdum = pl%s(ii, jj, kk, ee)%e(cc)
                            CASE('MS') ; rdum = pm%s(ii, jj, kk, ee)%m
                            END SELECT
                            rreal(d(1)%a,d(2)%a,d(3)%a,d(4)%a,d(5)%a,d(6)%a) = rdum
                        ENDIF
                    ENDDO
                ENDDO
            ENDDO
        ENDDO
    ENDDO
    END SUBROUTINE main_loop
        !cscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscsc
    PURE INTEGER FUNCTION FNS()
    IF(cc==1) THEN
        fns = pn%s(ii, jj, kk, ee)%m
    ELSEIF(cc>1 .AND. cc<6) THEN
        fns = pn%s(ii, jj, kk, ee)%b(cc-1)
    ELSE
        fns = pn%s(ii, jj, kk, ee)%r(cc-5)
    ENDIF
    END FUNCTION FNS
    !cfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfc
    PURE REAL FUNCTION FGS()
    IF(cc==1) THEN
        fgs = pg%s(ii, jj, kk, ee)%m
    ELSEIF(cc>1 .AND. cc<6) THEN
        fgs = pg%s(ii, jj, kk, ee)%b(cc-1)
    ELSE
        fgs = pg%s(ii, jj, kk, ee)%r(cc-5)
    ENDIF
    END FUNCTION FGS
END SUBROUTINE get_hdf5

!140805  following routines added to fix a memory leak (p%s was not being deallocated)
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE deall_pb(p)
TYPE(BS), POINTER :: p
DEALLOCATE(p%s)
NULLIFY(p%previous, p%next)
DEALLOCATE(p)
END SUBROUTINE deall_pb
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE deall_pe(p)
TYPE(ES), POINTER :: p
DEALLOCATE(p%s)
NULLIFY(p%previous, p%next)
DEALLOCATE(p)
END SUBROUTINE deall_pe
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE deall_pf(p)
TYPE(FS), POINTER :: p
DEALLOCATE(p%s)
NULLIFY(p%previous, p%next)
DEALLOCATE(p)
END SUBROUTINE deall_pf
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE deall_pg(p)
TYPE(GS), POINTER :: p
DEALLOCATE(p%s)
NULLIFY(p%previous, p%next)
DEALLOCATE(p)
END SUBROUTINE deall_pg
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE deall_pi(p)
TYPE(IS), POINTER :: p
DEALLOCATE(p%s)
NULLIFY(p%previous, p%next)
DEALLOCATE(p)
END SUBROUTINE deall_pi
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE deall_pl(p)
TYPE(LS), POINTER :: p
DEALLOCATE(p%s)
NULLIFY(p%previous, p%next)
DEALLOCATE(p)
END SUBROUTINE deall_pl
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE deall_pm(p)
TYPE(MS), POINTER :: p
DEALLOCATE(p%s)
NULLIFY(p%previous, p%next)
DEALLOCATE(p)
END SUBROUTINE deall_pm
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE deall_pn(p)
TYPE(NS), POINTER :: p
DEALLOCATE(p%s)
NULLIFY(p%previous, p%next)
DEALLOCATE(p)
END SUBROUTINE deall_pn

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
FUNCTION get_mbr(typ) RESULT(r)
INTEGER                             :: n
CHARACTER(2), INTENT(IN)            :: typ
CHARACTER(6), DIMENSION(:), POINTER :: r
CHARACTER(6), PARAMETER             :: sq(1)=(/'square'/),                            &
                                       bk(4)=(/'N-bank','E-bank','S-bank','W-bank'/), &
                                       rv(4)=(/'N-link','E-link','S-link','W-link'/)
n = MBR_COUNT(typ)
ALLOCATE(r(n))
SELECT CASE(typ)
CASE('BS') ; r = bk
CASE('ES') ; r = bk
CASE('FS') ; r = rv
CASE('GS') ; r = (/sq,bk,rv/)
CASE('IS') ; r = sq
CASE('LS') ; r = rv
CASE('MS') ; r = sq
CASE('NS') ; r = (/sq,bk,rv/)
END SELECT
END FUNCTION get_mbr

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
INTEGER FUNCTION TIME_COUNT(typ, first) RESULT(r)
INTEGER(INT_PTR_KIND()), INTENT(IN)      :: first
CHARACTER(*), INTENT(IN) :: typ
TYPE(BS), POINTER        :: pb
TYPE(ES), POINTER        :: pe
TYPE(FS), POINTER        :: pf
TYPE(GS), POINTER        :: pg
TYPE(IS), POINTER        :: pi
TYPE(LS), POINTER        :: pl
TYPE(MS), POINTER        :: pm
TYPE(NS), POINTER        :: pn
r = 1
llistend = first
SELECT CASE(typ)
CASE('BS') ; pb => llistend_b ; DO WHILE(ASSOCIATED(pb%next)) ; r=r+1 ; pb => pb%next ; ENDDO
CASE('ES') ; pe => llistend_e ; DO WHILE(ASSOCIATED(pb%next)) ; r=r+1 ; pe => pe%next ; ENDDO
CASE('FS') ; pf => llistend_f ; DO WHILE(ASSOCIATED(pb%next)) ; r=r+1 ; pf => pf%next ; ENDDO
CASE('GS') ; pg => llistend_g ; DO WHILE(ASSOCIATED(pg%next)) ; r=r+1 ; pg => pg%next ; ENDDO
CASE('IS') ; pi => llistend_i ; DO WHILE(ASSOCIATED(pi%next)) ; r=r+1 ; pi => pi%next ; ENDDO
CASE('LS') ; pl => llistend_l ; DO WHILE(ASSOCIATED(pl%next)) ; r=r+1 ; pl => pl%next ; ENDDO
CASE('MS') ; pm => llistend_m ; DO WHILE(ASSOCIATED(pm%next)) ; r=r+1 ; pm => pm%next ; ENDDO
CASE('NS') ; pn => llistend_n ; DO WHILE(ASSOCIATED(pn%next)) ; r=r+1 ; pn => pn%next ; ENDDO
END SELECT
END FUNCTION TIME_COUNT

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PURE INTEGER FUNCTION mbr_count(typ) RESULT(r)
!no of members
CHARACTER(*), INTENT(IN) :: typ
SELECT CASE(typ)
CASE('BS') ; r = 4
CASE('ES') ; r = 4
CASE('FS') ; r = 4
CASE('GS') ; r = 9
CASE('IS') ; r = 1
CASE('LS') ; r = 4
CASE('MS') ; r = 1
CASE('NS') ; r = 9
END SELECT
END FUNCTION mbr_count

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE save_items_worth_i(c, typ, a, b, klow, khigh, e, d, save_this, latest)
!DEC$ ATTRIBUTES DLLEXPORT :: save_items_worth_i
INTEGER, INTENT(IN)               :: a, b, klow, khigh, d, e
INTEGER(INT_PTR_KIND()), INTENT(IN)            :: latest
INTEGER, DIMENSION(khigh-klow+1), INTENT(IN) :: save_this  !23/0108  CVF IVF incompatibility
CHARACTER, INTENT(IN)             :: c
CHARACTER(*), INTENT(IN)          :: typ
SELECT CASE(typ)
    CASE('ES') ; llistend=latest ; CALL SAVE_ES(llistend_e, a, b, klow, khigh, e, d, save_this, c)
    CASE('FS') ; llistend=latest ; CALL SAVE_FS(llistend_f, a, b, klow, khigh, e, d, save_this, c)
    CASE('IS') ; llistend=latest ; CALL SAVE_IS(llistend_i, a, b, klow, khigh, e, d, save_this, c)
    CASE('NS') ; llistend=latest ; CALL SAVE_NS(llistend_n, a, b, klow, khigh, e, d, save_this, c)
END SELECT
END SUBROUTINE save_items_worth_i

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE save_items_worth_r(c, typ, a, b, klow, khigh, e, d, save_this, latest)
!DEC$ ATTRIBUTES DLLEXPORT :: save_items_worth_r
INTEGER, INTENT(IN)            :: a, b, klow, khigh, d, e
INTEGER(INT_PTR_KIND()), INTENT(IN)         :: latest
REAL, DIMENSION(khigh-klow+1), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)          :: c
CHARACTER(*), INTENT(IN)       :: typ
SELECT CASE(typ)
    CASE('BS') ; llistend=latest ; CALL SAVE_BS(llistend_b, a, b, klow, khigh, e, d, save_this,c)
    CASE('GS') ; llistend=latest ; CALL SAVE_GS(llistend_g, a, b, klow, khigh, e, d, save_this,c)
    CASE('LS') ; llistend=latest ; CALL SAVE_LS(llistend_l, a, b, klow, khigh, e, d, save_this,c)
    CASE('MS') ; llistend=latest ; CALL SAVE_MS(llistend_m, a, b, klow, khigh, e, d, save_this,c)
END SELECT
END SUBROUTINE save_items_worth_r

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
PURE SUBROUTINE save_bs(r, a, b, klow, khigh, e, d, save_this, c)
INTEGER, INTENT(IN)            :: a, b, klow, khigh, d, e
REAL, DIMENSION(:), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)          :: c
TYPE(BS), INTENT(INOUT)        :: r
r%s(a,b,klow:khigh,e)%e(d) = save_this
END SUBROUTINE save_bs
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
PURE SUBROUTINE save_es(r, a, b, klow, khigh, e, d, save_this, c)
INTEGER, INTENT(IN)               :: a, b, klow, khigh, d, e
INTEGER, DIMENSION(:), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)             :: c
TYPE(ES), INTENT(INOUT)           :: r
r%s(a,b,klow:khigh,e)%e(d) = save_this
END SUBROUTINE save_es
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
PURE SUBROUTINE save_fs(r, a, b, klow, khigh, e, d, save_this, c)
INTEGER, INTENT(IN)               :: a, b, klow, khigh, d, e
INTEGER, DIMENSION(:), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)             :: c
TYPE(FS), INTENT(INOUT)           :: r
r%s(a,b,klow:khigh,e)%e(d) = save_this
END SUBROUTINE save_fs
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
PURE SUBROUTINE save_gs(r, a, b, klow, khigh, e, d, save_this, c)
INTEGER, INTENT(IN)            :: a, b, klow, khigh, d, e
REAL, DIMENSION(:), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)          :: c
TYPE(GS), INTENT(INOUT)        :: r
SELECT CASE(c)
CASE('m') ; r%s(a,b,klow:khigh,e)%m    = save_this
CASE('b') ; r%s(a,b,klow:khigh,e)%b(d) = save_this
CASE('r') ; r%s(a,b,klow:khigh,e)%r(d) = save_this
END SELECT
END SUBROUTINE save_gs
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
PURE SUBROUTINE save_is(r, a, b, klow, khigh, e, d, save_this, c)
INTEGER, INTENT(IN)               :: a, b, klow, khigh, d, e
INTEGER, DIMENSION(:), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)             :: c
TYPE(IS), INTENT(INOUT)           :: r
r%s(a,b,klow:khigh,e)%m = save_this
END SUBROUTINE save_is
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
PURE SUBROUTINE save_ls(r, a, b, klow, khigh, e, d, save_this, c)
INTEGER, INTENT(IN)            :: a, b, klow, khigh, d, e
REAL, DIMENSION(:), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)          :: c
TYPE(LS), INTENT(INOUT)        :: r
r%s(a,b,klow:khigh,e)%e(d) = save_this
END SUBROUTINE save_ls
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
PURE SUBROUTINE save_ms(r, a, b, klow, khigh, e, d, save_this, c)
INTEGER, INTENT(IN)            :: a, b, klow, khigh, d, e
REAL, DIMENSION(:), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)          :: c
TYPE(MS), INTENT(INOUT)        :: r
r%s(a,b,klow:khigh,e)%m = save_this
END SUBROUTINE save_ms
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
PURE SUBROUTINE save_ns(r, a, b, klow, khigh, e, d, save_this, c)
INTEGER, INTENT(IN)               :: a, b, klow, khigh, d, e
INTEGER, DIMENSION(:), INTENT(IN) :: save_this
CHARACTER, INTENT(IN)             :: c
TYPE(NS), INTENT(INOUT)           :: r
SELECT CASE(c)
CASE('m') ; r%s(a,b,klow:khigh,e)%m = save_this
CASE('b') ; r%s(a,b,klow:khigh,e)%b(d) = save_this
CASE('r') ; r%s(a,b,klow:khigh,e)%r(d) = save_this
END SELECT
END SUBROUTINE save_ns


!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME(typ, time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
!DEC$ ATTRIBUTES DLLEXPORT :: FOR_NEW_TIME
INTEGER, INTENT(IN)      :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND()), INTENT(INOUT)   :: first, latest
REAL, INTENT(IN)         :: time
CHARACTER(*), INTENT(IN) :: typ
SELECT CASE(typ)
CASE('BS') ; CALL FOR_NEW_TIME_BS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
CASE('ES') ; CALL FOR_NEW_TIME_ES(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
CASE('GS') ; CALL FOR_NEW_TIME_GS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
CASE('IS') ; CALL FOR_NEW_TIME_IS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
CASE('LS') ; CALL FOR_NEW_TIME_LS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
CASE('MS') ; CALL FOR_NEW_TIME_MS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
CASE('NS') ; CALL FOR_NEW_TIME_NS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
END SELECT
END SUBROUTINE FOR_NEW_TIME

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME_BS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
INTEGER, INTENT(IN)    ::  ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND()), INTENT(INOUT) :: first, latest
REAL, INTENT(IN)       :: time
TYPE(BS), POINTER      :: r
ALLOCATE(r)
r%time =  time
ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
r%s = default_real_edges
IF(first==0) THEN
    first = LOC(r)
ELSE
    llistend        =  latest
    r%previous      => llistend_b
    llistend_b%next => r
ENDIF
latest = LOC(r)
END SUBROUTINE FOR_NEW_TIME_BS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME_ES(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND()), INTENT(INOUT) :: first, latest
REAL, INTENT(IN)       :: time
TYPE(ES), POINTER      :: r
ALLOCATE(r)
r%time =  time
ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
r%s = default_integer_edges
IF(first==0) THEN
    first = LOC(r)
ELSE
    llistend        =  latest
    r%previous      => llistend_e
    llistend_e%next => r
ENDIF
latest = LOC(r)
END SUBROUTINE FOR_NEW_TIME_ES
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME_FS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND()), INTENT(INOUT) :: first, latest
REAL, INTENT(IN)       :: time
TYPE(FS), POINTER      :: r
ALLOCATE(r)
r%time =  time
ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
r%s = default_integer_edges
IF(first==0) THEN
    first = LOC(r)
ELSE
    llistend        =  latest
    r%previous      => llistend_f
    llistend_f%next => r
ENDIF
latest = LOC(r)
END SUBROUTINE FOR_NEW_TIME_FS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME_GS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND()), INTENT(INOUT) :: first, latest
REAL, INTENT(IN)       :: time
TYPE(GS), POINTER      :: r
ALLOCATE(r)
r%time =  time
ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
r%s = default_real_middle_and_edges
IF(first==0) THEN
    first = LOC(r)
ELSE
    llistend        =  latest
    r%previous      => llistend_g
    llistend_g%next => r
ENDIF
latest = LOC(r)
END SUBROUTINE FOR_NEW_TIME_GS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME_IS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND()), INTENT(INOUT) :: first, latest
REAL, INTENT(IN)       :: time
TYPE(IS), POINTER      :: r
ALLOCATE(r)
r%time =  time
ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
r%s = default_integer_middle
IF(first==0) THEN
    first = LOC(r)
ELSE
    llistend        =  latest
    r%previous      => llistend_i
    llistend_i%next => r
ENDIF
latest = LOC(r)
END SUBROUTINE FOR_NEW_TIME_IS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME_LS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND()), INTENT(INOUT) :: first, latest
REAL, INTENT(IN)       :: time
TYPE(LS), POINTER      :: r
ALLOCATE(r)
r%time =  time
ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
r%s = default_real_edges
IF(first==0) THEN
    first = LOC(r)
ELSE
    llistend        =  latest
    r%previous      => llistend_l
    llistend_l%next => r
ENDIF
latest = LOC(r)
END SUBROUTINE FOR_NEW_TIME_LS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME_MS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND()), INTENT(INOUT) :: first, latest
REAL, INTENT(IN)       :: time
TYPE(MS), POINTER      :: r
ALLOCATE(r)
r%time =  time
ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
r%s = default_real_middle
IF(first==0) THEN
    first = LOC(r)
ELSE
    llistend        =  latest
    r%previous      => llistend_m
    llistend_m%next => r
ENDIF
latest = LOC(r)
END SUBROUTINE FOR_NEW_TIME_MS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE FOR_NEW_TIME_NS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
INTEGER(INT_PTR_KIND()), INTENT(INOUT) :: first, latest
REAL, INTENT(IN)       :: time
TYPE(NS), POINTER      :: r
ALLOCATE(r)
r%time =  time
ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
r%s = default_integer_middle_and_edges
IF(first==0) THEN
    first = LOC(r)
ELSE
    llistend        =  latest
    r%previous      => llistend_n
    llistend_n%next => r
ENDIF
latest = LOC(r)
END SUBROUTINE FOR_NEW_TIME_NS
END MODULE visualisation_structure

!!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
!FUNCTION get_hdf5_r(typ, sz, szo, first, ilow, jlow, klow) RESULT(r)
!INTEGER, INTENT(IN)                                  :: ilow, jlow, klow
!INTEGER, INTENT(INOUT)                               :: first
!INTEGER, DIMENSION(6), INTENT(IN)                    :: sz, szo
!INTEGER                                              :: szii, szjj, szkk, szcc, szee, sztt, &
!                                                        ii, jj, kk
!INTEGER, TARGET                                      :: dii, djj, dkk, cc, ee, tt
!REAL, DIMENSION(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)) :: r
!CHARACTER(*), INTENT(IN)                             :: typ
!TYPE(AORD), DIMENSION(:), POINTER, SAVE              :: d
!TYPE(BS), POINTER                                    :: pb
!TYPE(GS), POINTER                                    :: pg
!TYPE(LS), POINTER                                    :: pl
!TYPE(MS), POINTER                                    :: pm
!LOGICAL, SAVE                                        :: initial=T
!IF(initial) THEN
!    initial = F
!    ALLOCATE(d(6))
!ENDIF
!szii = sz(szo(1)) ; d(szo(1))%a=>dii
!!szjj = sz(szo(2)) ; d(szo(2))%a=>jj_inv
!szjj = sz(szo(2)) ; d(szo(2))%a=>djj
!szkk = sz(szo(3)) ; d(szo(3))%a=>dkk
!szcc = sz(szo(4)) ; d(szo(4))%a=>cc
!szee = sz(szo(5)) ; d(szo(5))%a=>ee
!sztt = sz(szo(6)) ; d(szo(6))%a=>tt
!r        = zero
!tt       = 1
!llistend = first
!SELECT CASE(TYP)
!CASE('BS')  !real banks
!    pb       => llistend_b
!    CALL MAIN_LOOP('BS')
!    IF(ASSOCIATED(pb%next)) THEN ; first = LOC(pb%next) ; DEALLOCATE(pb) ; ENDIF
!CASE('GS')  !real middle and edges
!    pg => llistend_g
!    CALL MAIN_LOOP('GS')
!    IF(ASSOCIATED(pg%next)) THEN ; first = LOC(pg%next) ; DEALLOCATE(pg) ; ENDIF
!CASE('LS')  !real banks
!    pl => llistend_l
!    CALL MAIN_LOOP('LS')
!    IF(ASSOCIATED(pl%next)) THEN ; first = LOC(pl%next) ; DEALLOCATE(pl) ; ENDIF
!CASE('MS')  !real middle
!    pm => llistend_m
!    CALL MAIN_LOOP('MS')
!    IF(ASSOCIATED(pm%next)) THEN ; first=LOC(pm%next) ; DEALLOCATE(pm) ; ENDIF
!END SELECT
!
!CONTAINS
!
!    !cscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscscsc
!    SUBROUTINE main_loop(text)  !there is a similar routine in get_hdf5_i
!    REAL                     :: dum
!    CHARACTER(*), INTENT(IN) :: text
!    DO dii=1,szii ; ii=ilow+dii-1
!        DO djj=1,szjj ; jj=jlow+djj-1
!            DO dkk=1,szkk ; kk=klow+dkk-1
!                DO ee=1,szee
!                    DO cc=1,szcc
!                        SELECT CASE(text)
!                        CASE('BS') ; dum = FBS()
!                        CASE('GS') ; dum = FGS()
!                        CASE('LS') ; dum = FLS()
!                        CASE('MS') ; dum = FMS()
!                        END SELECT
!                        r(d(1)%a,d(2)%a,d(3)%a,d(4)%a,d(5)%a,d(6)%a) = dum
!                    ENDDO
!                ENDDO
!            ENDDO
!        ENDDO
!    ENDDO
!    END SUBROUTINE main_loop
!    !cfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfc
!    PURE REAL FUNCTION FBS()
!        fbs = pb%s(ii, jj, kk, ee)%e(cc)
!    END FUNCTION FBS
!    !cfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfc
!    PURE REAL FUNCTION FGS()
!    IF(cc==1) THEN
!        fgs = pg%s(ii, jj, kk, ee)%m
!    ELSEIF(cc>1 .AND. cc<6) THEN
!        fgs = pg%s(ii, jj, kk, ee)%b(cc-1)
!    ELSE
!        fgs = pg%s(ii, jj, kk, ee)%r(cc-5)
!    ENDIF
!    END FUNCTION FGS
!    !cfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfc
!    PURE REAL FUNCTION FLS()
!        fls = pl%s(ii, jj, kk, ee)%e(cc)
!    END FUNCTION FLS
!    !cfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfcfc
!    PURE REAL FUNCTION FMS()
!        fms = pm%s(ii, jj, kk, ee)%m
!    END FUNCTION FMS
!
!END FUNCTION get_hdf5_r

!!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!SUBROUTINE dump(name, typ, i, time, first, isgrid)
!INTEGER, INTENT(IN)      :: i, first
!INTEGER, PARAMETER       :: ii=7, jj=12
!REAL, INTENT(IN)         :: time
!LOGICAL, INTENT(IN)      :: isgrid
!CHARACTER(*), INTENT(IN) :: name, typ
!CHARACTER(20), PARAMETER ::fff='(100F12.4)', fffi='(100I8)'
!TYPE(BS), POINTER       :: pb
!TYPE(ES), POINTER       :: pe
!TYPE(FS), POINTER       :: pf
!TYPE(GS), POINTER       :: pg
!TYPE(IS), POINTER       :: pi
!TYPE(LS), POINTER       :: pl
!TYPE(MS), POINTER       :: pm
!TYPE(NS), POINTER       :: pn
!llistend = first
!WRITE(654,*) ; WRITE(654,'(A, I4,A,F15.3)') TRIM(name)//'  '//typ//' ', i,'  time = ',time
!SELECT CASE(typ)
!CASE('BS')
!    llistend = first
!    pb       => llistend_b
!    WRITE(654,fff) pb%time, pb%s(ii,jj,UBOUND(pb%s, DIM=3),:)
!    DO WHILE(ASSOCIATED(pb%next))
!        pb => pb%next
!        WRITE(654,fff) pb%time, pb%s(ii,jj,UBOUND(pb%s, DIM=3),:)
!    ENDDO
!CASE('ES')
!    llistend = first
!    pe       => llistend_e
!    WRITE(654,fffi) pe%time, pe%s(ii,jj,UBOUND(pe%s, DIM=3),:)
!    DO WHILE(ASSOCIATED(pb%next))
!        pe => pe%next
!        WRITE(654,fffi) pe%time, pe%s(ii,jj,UBOUND(pe%s, DIM=3),:)
!    ENDDO
!CASE('FS')
!    llistend = first
!    pf       => llistend_f
!    WRITE(654,fffi) pf%time, pf%s(ii,jj,UBOUND(pf%s, DIM=3),:)
!    DO WHILE(ASSOCIATED(pb%next))
!        pf => pf%next
!        WRITE(654,fffi) pf%time, pf%s(ii,jj,UBOUND(pf%s, DIM=3),:)
!    ENDDO
!CASE('GS')
!    llistend = first
!    pg       => llistend_g
!    WRITE(654,fff) pg%time, pg%s(ii,jj,UBOUND(pg%s, DIM=3),:)
!    DO WHILE(ASSOCIATED(pg%next))
!        pg => pg%next
!        WRITE(654,fff) pg%time, pg%s(ii,jj,UBOUND(pg%s, DIM=3),:)
!    ENDDO
!CASE('IS')
!    llistend = first
!    pi       => llistend_i
!    WRITE(654,fffi) pi%time, pi%s(ii,jj,UBOUND(pi%s, DIM=3),:)
!    DO WHILE(ASSOCIATED(pi%next))
!        pi => pi%next
!        WRITE(654,fffi) pi%time, pi%s(ii,jj,UBOUND(pi%s, DIM=3),:)
!    ENDDO
!CASE('LS')
!    llistend = first
!    pl       => llistend_l
!    WRITE(654,fff) pl%time, pl%s(ii,jj,UBOUND(pl%s, DIM=3),:)
!    DO WHILE(ASSOCIATED(pl%next))
!        pl => pl%next
!        WRITE(654,fff) pl%time, pl%s(ii,jj,UBOUND(pl%s, DIM=3),:)
!    ENDDO
!CASE('MS')
!    llistend = first
!    pm       => llistend_m
!    IF(isgrid) THEN
!        WRITE(654,fff) pm%time, pm%s(ii,jj,UBOUND(pm%s, DIM=3),:)
!    ELSE
!        WRITE(654,fff) pm%time, pm%s(:,:,UBOUND(pm%s, DIM=3),:)
!    ENDIF
!    DO WHILE(ASSOCIATED(pm%next))
!        pm => pm%next
!            IF(isgrid) THEN
!        WRITE(654,fff) pm%time, pm%s(ii,jj,UBOUND(pm%s, DIM=3),:)
!    ELSE
!        WRITE(654,fff) pm%time, pm%s(:,:,UBOUND(pm%s, DIM=3),:)
!    ENDIF
!    ENDDO
!CASE('NS')
!    llistend = first
!    pn       => llistend_n
!    WRITE(654,fffi) pn%time, pn%s(ii,jj,UBOUND(pn%s, DIM=3),:)
!    DO WHILE(ASSOCIATED(pn%next))
!        pn => pn%next
!        WRITE(654,fffi) pn%time, pn%s(ii,jj,UBOUND(pn%s, DIM=3),:)
!    ENDDO
!END SELECT
!END SUBROUTINE dump