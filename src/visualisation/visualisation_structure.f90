!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
MODULE visualisation_structure

   USE ISO_C_BINDING, ONLY: C_PTR, C_NULL_PTR, C_LOC, C_F_POINTER, C_ASSOCIATED

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


!EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE EDGE
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
   TYPE integer_edges !for middle of cell and edges
      INTEGER :: e(4) = iundef    !edge N, E, S, W
   END TYPE integer_edges
   TYPE(INTEGER_EDGES), PARAMETER :: &
      default_integer_edges = INTEGER_EDGES(defi4)
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
   TYPE real_edges !for middle of cell and edges
      REAL :: e(4) = rundef  !edges N, E, S, W
   END TYPE real_edges
   TYPE(REAL_EDGES), PARAMETER :: &
      default_real_edges = REAL_EDGES(defr4)
!MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE MIDDLE
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
   TYPE integer_middle !for middle of cell
      INTEGER :: m = rundef  !middle
   END TYPE integer_middle
   TYPE(INTEGER_MIDDLE), PARAMETER :: &
      default_integer_middle = INTEGER_MIDDLE(r_not_exist)
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
   TYPE real_middle !for middle of cell
      REAL :: m    = rundef  !middle
   END TYPE real_middle
   TYPE(REAL_MIDDLE), PARAMETER :: &
      default_real_middle = REAL_MIDDLE(r_not_exist)
!MIDDLE AND EDGES MIDDLE AND EDGES MIDDLE AND EDGES MIDDLE AND EDGES MIDDLE AND EDGES
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
   TYPE integer_middle_and_edges !for middle of cell and edges
      PRIVATE
      INTEGER :: m    = iundef, &  !middle
         b(4) = iundef, &  !bank N, E, S, W
         r(4) = iundef     !river N, E, S, W
   END TYPE integer_middle_and_edges
   TYPE(INTEGER_MIDDLE_AND_EDGES), PARAMETER :: &
      default_integer_middle_and_edges = INTEGER_MIDDLE_AND_EDGES(i_not_exist, defi4, defi4)
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
   TYPE real_middle_and_edges !for middle of cell and edges
! sequence
      REAL :: m    = rundef, &  !middle
         b(4) = rundef, &  !bank N, E, S, W
         r(4) = rundef     !river N, E, S, W
   END TYPE real_middle_and_edges
   TYPE(REAL_MIDDLE_AND_EDGES), PARAMETER :: &
      default_real_middle_and_edges = REAL_MIDDLE_AND_EDGES(r_not_exist, defr4, defr4)


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
! sequence
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


   INTERFACE SAVE_ITEMS_WORTH ; MODULE PROCEDURE SAVE_ITEMS_WORTH_I, SAVE_ITEMS_WORTH_R ; ENDINTERFACE

   PRIVATE
   PUBLIC :: FOR_NEW_TIME, SAVE_ITEMS_WORTH, TIME_COUNT, MBR_COUNT, GET_MBR, GET_HDF5_I, GET_HDF5_R, &
      GET_HDF5_TIME, csz


CONTAINS

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
   REAL FUNCTION get_hdf5_time(typ, first) RESULT(r)

      TYPE(C_PTR), INTENT(INOUT) :: first
      CHARACTER(*), INTENT(IN)   :: typ
      TYPE(BS), POINTER          :: pb
      TYPE(ES), POINTER          :: pe
      TYPE(FS), POINTER          :: pf
      TYPE(GS), POINTER          :: pg
      TYPE(IS), POINTER          :: pi
      TYPE(LS), POINTER          :: pl
      TYPE(MS), POINTER          :: pm
      TYPE(NS), POINTER          :: pn

      SELECT CASE(typ)
       CASE('BS') ; CALL C_F_POINTER(first, pb) ; r = pb%time
       CASE('ES') ; CALL C_F_POINTER(first, pe) ; r = pe%time
       CASE('FS') ; CALL C_F_POINTER(first, pf) ; r = pf%time
       CASE('GS') ; CALL C_F_POINTER(first, pg) ; r = pg%time
       CASE('IS') ; CALL C_F_POINTER(first, pi) ; r = pi%time
       CASE('LS') ; CALL C_F_POINTER(first, pl) ; r = pl%time
       CASE('MS') ; CALL C_F_POINTER(first, pm) ; r = pm%time
       CASE('NS') ; CALL C_F_POINTER(first, pn) ; r = pn%time
      END SELECT
   END FUNCTION get_hdf5_time


!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
   SUBROUTINE get_hdf5_i(typ, sz, szo, first, ilow, jlow, klow, r)
      INTEGER, INTENT(IN)                                     :: ilow, jlow, klow
      TYPE(C_PTR), INTENT(INOUT)                              :: first
      INTEGER, DIMENSION(6), INTENT(IN)                       :: sz, szo
      INTEGER, DIMENSION(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)), INTENT(OUT) :: r
      CHARACTER(*), INTENT(IN)                                :: typ
      CALL GET_HDF5(typ, sz, szo, first, ilow, jlow, klow, rint=r)
   END SUBROUTINE get_hdf5_i

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
   SUBROUTINE get_hdf5_r(typ, sz, szo, first, ilow, jlow, klow, r)
      INTEGER, INTENT(IN)                                     :: ilow, jlow, klow
      TYPE(C_PTR), INTENT(INOUT)                              :: first
      INTEGER, DIMENSION(6), INTENT(IN)                       :: sz, szo
      REAL, DIMENSION(sz(1),sz(2),sz(3),sz(4),sz(5),sz(6)), INTENT(OUT)    :: r
      CHARACTER(*), INTENT(IN)                                :: typ
      CALL GET_HDF5(typ, sz, szo, first, ilow, jlow, klow, rreal=r)
   END SUBROUTINE get_hdf5_r

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE get_hdf5(typ, sz, szo, first, ilow, jlow, klow, rint, rreal)
      INTEGER, INTENT(IN)                     :: ilow, jlow, klow
      TYPE(C_PTR), INTENT(INOUT)              :: first
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
      TYPE(GS), POINTER        :: pg
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

      SELECT CASE(TYP)
       CASE('BS')  !real banks
         CALL C_F_POINTER(first, pb)
         CALL MAIN_LOOP('BS')
         IF(ASSOCIATED(pb%next)) THEN ; first = C_LOC(pb%next) ; ELSE ; first = C_NULL_PTR ; ENDIF
         CALL DEALL_PB(pb)
       CASE('ES')  !integer banks
         CALL C_F_POINTER(first, pe)
         CALL MAIN_LOOP('ES')
         IF(ASSOCIATED(pe%next)) THEN ; first = C_LOC(pe%next) ;  ELSE ; first=C_NULL_PTR ; ENDIF
         CALL DEALL_PE(pe)
       CASE('FS')  !integer rivers
         CALL C_F_POINTER(first, pf)
         CALL MAIN_LOOP('FS')
         IF(ASSOCIATED(pf%next)) THEN ; first = C_LOC(pf%next) ;  ELSE ; first=C_NULL_PTR ; ENDIF
         CALL DEALL_PF(pf)
       CASE('GS')  !real middle and edges
         CALL C_F_POINTER(first, pg)
         CALL MAIN_LOOP('GS')
         IF(ASSOCIATED(pg%next)) THEN ; first = C_LOC(pg%next) ;  ELSE ; first=C_NULL_PTR ; ENDIF
         CALL DEALL_PG(pg)
       CASE('IS')  !integer middle
         CALL C_F_POINTER(first, pi)
         CALL MAIN_LOOP('IS')
         IF(ASSOCIATED(pi%next)) THEN ; first = C_LOC(pi%next) ;  ELSE ; first=C_NULL_PTR ; ENDIF
         CALL DEALL_PI(pi)
       CASE('LS')  !real banks
         CALL C_F_POINTER(first, pl)
         CALL MAIN_LOOP('LS')
         IF(ASSOCIATED(pl%next)) THEN ; first = C_LOC(pl%next) ;  ELSE ; first=C_NULL_PTR ; ENDIF
         CALL DEALL_PL(pl)
       CASE('MS')  !real middle
         CALL C_F_POINTER(first, pm)
         CALL MAIN_LOOP('MS')
         IF(ASSOCIATED(pm%next)) THEN ; first = C_LOC(pm%next) ;  ELSE ; first=C_NULL_PTR ; ENDIF
         CALL DEALL_PM(pm)
       CASE('NS')  !integer middle and edges
         CALL C_F_POINTER(first, pn)
         CALL MAIN_LOOP('NS')
         IF(ASSOCIATED(pn%next)) THEN ; first = C_LOC(pn%next) ;  ELSE ; first=C_NULL_PTR ; ENDIF
         CALL DEALL_PN(pn)
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
      TYPE(C_PTR), INTENT(INOUT) :: first
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
      SELECT CASE(typ)
       CASE('BS') ; CALL C_F_POINTER(first, pb) ; DO WHILE(ASSOCIATED(pb%next)) ; r=r+1 ; pb => pb%next ; ENDDO
       CASE('ES') ; CALL C_F_POINTER(first, pe) ; DO WHILE(ASSOCIATED(pe%next)) ; r=r+1 ; pe => pe%next ; ENDDO
       CASE('FS') ; CALL C_F_POINTER(first, pf) ; DO WHILE(ASSOCIATED(pf%next)) ; r=r+1 ; pf => pf%next ; ENDDO
       CASE('GS') ; CALL C_F_POINTER(first, pg) ; DO WHILE(ASSOCIATED(pg%next)) ; r=r+1 ; pg => pg%next ; ENDDO
       CASE('IS') ; CALL C_F_POINTER(first, pi) ; DO WHILE(ASSOCIATED(pi%next)) ; r=r+1 ; pi => pi%next ; ENDDO
       CASE('LS') ; CALL C_F_POINTER(first, pl) ; DO WHILE(ASSOCIATED(pl%next)) ; r=r+1 ; pl => pl%next ; ENDDO
       CASE('MS') ; CALL C_F_POINTER(first, pm) ; DO WHILE(ASSOCIATED(pm%next)) ; r=r+1 ; pm => pm%next ; ENDDO
       CASE('NS') ; CALL C_F_POINTER(first, pn) ; DO WHILE(ASSOCIATED(pn%next)) ; r=r+1 ; pn => pn%next ; ENDDO
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
       CASE DEFAULT ; r = 0
      END SELECT
   END FUNCTION mbr_count

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE save_items_worth_i(c, typ, a, b, klow, khigh, e, d, save_this, latest)
      INTEGER, INTENT(IN)               :: a, b, klow, khigh, d, e
      TYPE(C_PTR), INTENT(IN)           :: latest
      TYPE(ES), POINTER                 :: ptr_e
      TYPE(FS), POINTER                 :: ptr_f
      TYPE(IS), POINTER                 :: ptr_i
      TYPE(NS), POINTER                 :: ptr_n
      INTEGER, DIMENSION(khigh-klow+1), INTENT(IN) :: save_this  !23/0108  CVF IVF incompatibility
      CHARACTER, INTENT(IN)             :: c
      CHARACTER(*), INTENT(IN)          :: typ
      SELECT CASE(typ)
       CASE('ES') ; CALL C_F_POINTER(latest, ptr_e) ; CALL SAVE_ES(ptr_e, a, b, klow, khigh, e, d, save_this)
       CASE('FS') ; CALL C_F_POINTER(latest, ptr_f) ; CALL SAVE_FS(ptr_f, a, b, klow, khigh, e, d, save_this)
       CASE('IS') ; CALL C_F_POINTER(latest, ptr_i) ; CALL SAVE_IS(ptr_i, a, b, klow, khigh, e, save_this)
       CASE('NS') ; CALL C_F_POINTER(latest, ptr_n) ; CALL SAVE_NS(ptr_n, a, b, klow, khigh, e, d, save_this, c)
      END SELECT
   END SUBROUTINE save_items_worth_i

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE save_items_worth_r(c, typ, a, b, klow, khigh, e, d, save_this, latest)
      INTEGER, INTENT(IN)            :: a, b, klow, khigh, d, e
      TYPE(C_PTR), INTENT(IN)           :: latest
      TYPE(BS), POINTER                 :: ptr_b
      TYPE(GS), POINTER                 :: ptr_g
      TYPE(LS), POINTER                 :: ptr_l
      TYPE(MS), POINTER                 :: ptr_m
      REAL, DIMENSION(khigh-klow+1), INTENT(IN) :: save_this
      CHARACTER, INTENT(IN)          :: c
      CHARACTER(*), INTENT(IN)       :: typ
      SELECT CASE(typ)
       CASE('BS') ; CALL C_F_POINTER(latest, ptr_b) ; CALL SAVE_BS(ptr_b, a, b, klow, khigh, e, d, save_this)
       CASE('GS') ; CALL C_F_POINTER(latest, ptr_g) ; CALL SAVE_GS(ptr_g, a, b, klow, khigh, e, d, save_this,c)
       CASE('LS') ; CALL C_F_POINTER(latest, ptr_l) ; CALL SAVE_LS(ptr_l, a, b, klow, khigh, e, d, save_this)
       CASE('MS') ; CALL C_F_POINTER(latest, ptr_m) ; CALL SAVE_MS(ptr_m, a, b, klow, khigh, e, save_this)
      END SELECT
   END SUBROUTINE save_items_worth_r

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   PURE SUBROUTINE save_bs(r, a, b, klow, khigh, e, d, save_this)
      INTEGER, INTENT(IN)            :: a, b, klow, khigh, d, e
      REAL, DIMENSION(:), INTENT(IN) :: save_this
      TYPE(BS), INTENT(INOUT)        :: r
      r%s(a,b,klow:khigh,e)%e(d) = save_this
   END SUBROUTINE save_bs
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   PURE SUBROUTINE save_es(r, a, b, klow, khigh, e, d, save_this)
      INTEGER, INTENT(IN)               :: a, b, klow, khigh, d, e
      INTEGER, DIMENSION(:), INTENT(IN) :: save_this
      TYPE(ES), INTENT(INOUT)           :: r
      r%s(a,b,klow:khigh,e)%e(d) = save_this
   END SUBROUTINE save_es
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   PURE SUBROUTINE save_fs(r, a, b, klow, khigh, e, d, save_this)
      INTEGER, INTENT(IN)               :: a, b, klow, khigh, d, e
      INTEGER, DIMENSION(:), INTENT(IN) :: save_this
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
   PURE SUBROUTINE save_is(r, a, b, klow, khigh, e, save_this)
      INTEGER, INTENT(IN)               :: a, b, klow, khigh, e
      INTEGER, DIMENSION(:), INTENT(IN) :: save_this
      TYPE(IS), INTENT(INOUT)           :: r
      r%s(a,b,klow:khigh,e)%m = save_this
   END SUBROUTINE save_is
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   PURE SUBROUTINE save_ls(r, a, b, klow, khigh, e, d, save_this)
      INTEGER, INTENT(IN)            :: a, b, klow, khigh, d, e
      REAL, DIMENSION(:), INTENT(IN) :: save_this
      TYPE(LS), INTENT(INOUT)        :: r
      r%s(a,b,klow:khigh,e)%e(d) = save_this
   END SUBROUTINE save_ls
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   PURE SUBROUTINE save_ms(r, a, b, klow, khigh, e, save_this)
      INTEGER, INTENT(IN)            :: a, b, klow, khigh, e
      REAL, DIMENSION(:), INTENT(IN) :: save_this
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
      INTEGER, INTENT(IN)      :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
      TYPE(C_PTR), INTENT(INOUT)   :: first, latest
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
      TYPE(C_PTR), INTENT(INOUT) :: first, latest
      REAL, INTENT(IN)       :: time
      TYPE(BS), POINTER      :: r, prev_node
      ALLOCATE(r)
      r%time =  time
      ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
      r%s = default_real_edges
      IF(.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous      => prev_node
         prev_node%next => r
      ENDIF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_BS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE FOR_NEW_TIME_ES(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
      TYPE(C_PTR), INTENT(INOUT) :: first, latest
      REAL, INTENT(IN)       :: time
      TYPE(ES), POINTER      :: r, prev_node
      ALLOCATE(r)
      r%time =  time
      ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
      r%s = default_integer_edges
      IF(.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous      => prev_node
         prev_node%next => r
      ENDIF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_ES
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   ! SUBROUTINE FOR_NEW_TIME_FS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
   !    INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
   !    TYPE(C_PTR), INTENT(INOUT) :: first, latest
   !    REAL, INTENT(IN)       :: time
   !    TYPE(FS), POINTER      :: r, prev_node

   !    ALLOCATE(r)
   !    r%time =  time

   !    ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
   !    r%s = default_integer_edges

   !    IF(.NOT. C_ASSOCIATED(first)) THEN
   !       first = C_LOC(r)
   !    ELSE
   !       CALL c_f_pointer(latest, prev_node)
   !       r%previous      => prev_node
   !       prev_node%next => r
   !    ENDIF
   !    latest = C_LOC(r)
   ! END SUBROUTINE FOR_NEW_TIME_FS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE FOR_NEW_TIME_GS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN) :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
      TYPE(C_PTR), INTENT(INOUT) :: first, latest
      REAL, INTENT(IN) :: time
      TYPE(GS), POINTER :: r, prev_node

      ALLOCATE(r)
      r%time = time

      ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
      r%s = default_real_middle_and_edges

      IF (.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         ! Safely convert the 'latest' generic pointer back to a GS pointer
         CALL C_F_POINTER(latest, prev_node)
         r%previous => prev_node
         prev_node%next => r
      ENDIF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_GS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE FOR_NEW_TIME_IS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
      TYPE(C_PTR), INTENT(INOUT) :: first, latest
      REAL, INTENT(IN)       :: time
      TYPE(IS), POINTER      :: r, prev_node
      ALLOCATE(r)
      r%time =  time
      ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
      r%s = default_integer_middle
      IF(.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous      => prev_node
         prev_node%next => r
      ENDIF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_IS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE FOR_NEW_TIME_LS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
      TYPE(C_PTR), INTENT(INOUT) :: first, latest
      REAL, INTENT(IN)       :: time
      TYPE(LS), POINTER      :: r, prev_node
      ALLOCATE(r)
      r%time =  time
      ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
      r%s = default_real_edges
      IF(.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous      => prev_node
         prev_node%next => r
      ENDIF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_LS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE FOR_NEW_TIME_MS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
      TYPE(C_PTR), INTENT(INOUT) :: first, latest
      REAL, INTENT(IN)       :: time
      TYPE(MS), POINTER      :: r, prev_node
      ALLOCATE(r)
      r%time =  time
      ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
      r%s = default_real_middle
      IF(.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous      => prev_node
         prev_node%next => r
      ENDIF
      latest = C_LOC(r)
   END SUBROUTINE FOR_NEW_TIME_MS
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   SUBROUTINE FOR_NEW_TIME_NS(time, ilow, ihigh, jlow, jhigh, klow, khigh, ext, first, latest)
      INTEGER, INTENT(IN)    :: ilow, ihigh, jlow, jhigh, klow, khigh, ext
      TYPE(C_PTR), INTENT(INOUT) :: first, latest
      REAL, INTENT(IN)       :: time
      TYPE(NS), POINTER      :: r, prev_node
      ALLOCATE(r)
      r%time =  time
      ALLOCATE(r%s(ilow:ihigh,jlow:jhigh,klow:khigh,ext))
      r%s = default_integer_middle_and_edges
      IF(.NOT. C_ASSOCIATED(first)) THEN
         first = C_LOC(r)
      ELSE
         CALL c_f_pointer(latest, prev_node)
         r%previous      => prev_node
         prev_node%next => r
      ENDIF
      latest = C_LOC(r)
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
