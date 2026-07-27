!MMMMMM MODULE ocqdqmod
MODULE ocqdqmod
! JE  1/09   4.3.5F90  Created, as part of conversion to FORTRAN90
!***ZQ Module 200520
! new variables     NoZQTables,ZQTableRef,ZQTableLink,ZQTableFace
   USE SGLOBAL
   USE AL_C ,     ONLY : ICMRF2, CWIDTH, DHF, ZBFULL, CLENTH
   USE AL_G ,     ONLY : ICMREF, ICMXY
   USE AL_D ,     ONLY : DQ0ST, DQIST, DQIST2, NOCBCC, NOCBCD, NoZQTables,ZQTableRef, ZQTableLink,ZQTableFace
   USE OCmod2 ,   ONLY : GETHRF, OCQMLN, SETQSA, OCQBNK, OCQGRD, OCQLNK, OCQBC

   IMPLICIT NONE
   DOUBLEPRECISION    :: XAFULL(NLFEE), COCBCD(5, NOCTAB)
   DOUBLEPRECISION    :: HOCNOW (NOCTAB), QOCF (NOCTAB)
   DOUBLEPRECISION    :: STRXX(NELEE), STRYY(NELEE)
!LOGICAL            :: firstocqdq=.TRUE.


   PRIVATE
   PUBLIC :: OCQDQ, STRXX, STRYY, HOCNOW, QOCF, XAFULL, COCBCD ! , firstocqdq

CONTAINS

   !SSSSSS SUBROUTINE OCQDQ
   SUBROUTINE OCQDQ ()
   !----------------------------------------------------------------------*
   !
   !  CONTROL CALCULATION OF FLOWS AND DERIVATIVES AT ELEMENT FACES
   !
   !----------------------------------------------------------------------*
   ! Version:  SHETRAN/OC/OCQDQ/4.2
   ! Modifications:
   ! RAH  941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL.
   ! RAH  980224  4.2  Bring JEL2,JFACE2,ZI,ZGI from OCQMLN, and pass
   !                   as arguments - along with PRI,LI - in place of
   !                   locals IFACE,JEL,FIRST,DDDZ2.  Also: set only JEL2
   !                   at null branches; call on lowest IEL, not IROW,IND;
   !                   rewrite loop 260.  Explicit typing.
   !      980225       Call OCQBNK,OCQGRD,OCQLNK on lowest IEL.  Don't
   !                   initialize Q,DQ0,DQI.  Use IEL.le.NLF for ITYPE=3.
   !                   Rename INDEX I/JBC, & set IBC once only; also use
   !                   new local NBC.  Set JFACE only if JEL.GT.IEL.
   !                   Restructure IF-blocks to avoid unnecessary steps.
   !                   Don't pass JFACE2 to OCQMLN.  Full args for OCQBC.
   !      980226       One call; loop over IEL; scrap args ICOUNT,IROW.
   !                   Move COCBCD,QOCF to arg list; don't INCLUDE SPEC.OC.
   !      980327       New input argument XAFULL (see OCSIM).
   !      980331       OCQGRD: remove input args I/JEL,I/JFACE,INDEX & add
   !                   W (new local),LI,ZGI,STR,ZI; replace output vars
   !                   Q,DQ0,DQI with arrays QJ,DQ; remove output DDDZ.
   !                   New statement functions FSTR,FDQQ & args STRX,STRY.
   !      980406       OCQLNK: remove input args I/JFACE,NBC & add
   !                   LI,ZGI,COCBCD,ZI; outputs as OCQGRD above.
   !      980408       OCQBNK: remove input args I/JEL,I/JFACE & add
   !                   CLENTH,LI,ZGI,STR,ZI; outputs as OCQGRD above.
   !                   Replace remaining Q,DQ0,DQI with QJ,DQ.
   !      980409       OCQBC args: remove output DQ(0,1) & set to zero; add
   !                   inputs W,STR, & HOCNOW (new, see OCSIM) & re-order.
   !      980424       Add arguments NXSCEE,XSTAB (see OCSIM).
   !                   Pass NXSCEE,STR,CW,XA to OCQMLN,OCQLNK.
   !                   OCQLNK args also: remove IEL,JEL; move NTYPE.
   !      980427       Pass new work arg XS (see OCSIM) to OCQMLN,OCQLNK.
   !                   OCQBC args: add NXSCEE,XAFULL,XSTAB; rm IEL,IFACE.
   !                   Reorder OCQGRD arguments.
   !      980807       New local LINK (else, strictly, get out-of-bounds).
   !----------------------------------------------------------------------*
   ! Entry requirements:
   !   NELEE.ge.NEL    [NEL,NXSCEE].ge.1    NLFEE.ge.[1,-ICMREF(1:NEL,5:8)]
   !  NOCTAB.ge.[1,NOCBCC(1:NEL)]
   !  for i in {ICMREF(1:NEL,5:8).lt.0}:  ICMRF2(-i,1:3).le.NEL
   !               ICMRF2(-i,4;6).ge.1    ICMRF2(-i,4;6).le.4
   !  for iel in 1:NEL
   !    { let ibc=NOCBCC(iel), face=NOCBCD(ibc,2), jel=ICMREF(iel,face) ;
   !      if ibc.gt.0 then
   !   ( jel.ge.0 .and. ( jel.eq.0 .or. (jel.le.NLF).eqv.(iel.le.NLF) ) ) }
   !
   !980225 Disallow conflicts when NOCBCC(iel),NOCBCC(jel) both non-zero
   !980225 Require consistency of ICMREF/ICMRF2 - see loop 260
   !----------------------------------------------------------------------*
   ! Commons and constants
   ! Imported constants
   !     SPEC.AL          NELEE,NLFEE,NOCTAB
   ! Input common
   !     SPEC.AL          PRI,NEL,NLF
   !                      ICMREF(NELEE,5:12),NOCBCC(NEL)
   !                      ICMRF2(NLFEE,6),   NOCBCD(NOCTAB,2:3)
   !                      DYQQ(NEL),CWIDTH(NLFEE),ZGRUND(NEL),DHF(NELEE,4)
   !                      DXQQ(NEL),CLENTH(NLFEE),ZBFULL(NLFEE),  HRF(NEL)
   ! Output common
   !     SPEC.AL            QSA(NELEE,4),DQIST(NELEE,4)
   !                      DQ0ST(NELEE,4),DQIST2(NLFEE,3)
   !                Note: Usually expect DQ0ST<0 and DQIST*>0.
   ! Version:  SHETRAN/OC/OCQDQ/4.2

      ! Assumed external module dependencies providing global variables:
      ! NOCBCC, NOCBCD, ICMREF, ICMRF2, total_no_elements, total_no_links,
      ! zero, FDQQ, FSTR, DHF, ZGRUND, XAFULL, COCBCD, GETHRF, HOCNOW, QOCF,
      ! STRXX, CWIDTH, CLENTH, ZBFULL, NoZQTables, ZQTableLink, ZQTableFace,
      ! ZQTableRef, DQ0ST, DQIST, DQIST2, OCQBC, OCQBNK, OCQGRD, OCQLNK, OCQMLN, SETQSA

      IMPLICIT NONE

      ! Locals
      INTEGER                         :: i, IBC, IBR, IELu, IFACE, ICAT, NBC, NTYPE, NFACE
      INTEGER                         :: JBC, JBR, JEL, JFACE, J, JJ, JJJ, JMAX, KEL, KFACE, LINK, itemp
      INTEGER                         :: jxswork(0:3)
      INTEGER, DIMENSION(0:3)         :: JEL2, JFACE2
      DOUBLE PRECISION, DIMENSION(0:3):: CW, LI, STR, QJ, XA, ZI, ZGI
      DOUBLE PRECISION                :: DQ(0:1,0:1), DQIJ(0:3,0:3)
      DOUBLE PRECISION                :: W
      LOGICAL                         :: MULTI, SINGLE, eexternal

      !----------------------------------------------------------------------*

      QJ = zero
      DQ = zero

      element_loop: DO ielu = 1, total_no_elements
         
         IBC = NOCBCC(ielu)  ! ----- BC index and face number
         IF (IBC > 0) THEN
            NFACE = NOCBCD(IBC, 2)
         ELSE
            NFACE = 0
         END IF

         face_loop: DO IFACE = 1, 4
            
            JEL = ICMREF(ielu, IFACE + 4)
            SINGLE = JEL > 0
            
            IF (JEL < ielu .AND. SINGLE) CYCLE face_loop
            
            MULTI = JEL < 0
            eexternal = JEL == 0
            
            IF (eexternal) THEN
               IF (NFACE == IFACE) THEN
                  W = FDQQ(ielu, IFACE)
                  STR(0) = FSTR(ielu, IFACE)
                  NTYPE = NOCBCD(IBC, 3)
                  ICAT = NOCBCD(IBC, 4)
                  LINK = MAX(1, MIN(ielu, total_no_links))
                  
                  ! PERF FIX: Pass base memory address COCBCD(1, IBC) to avoid dope vector overhead
                  CALL OCQBC(NTYPE, DHF(ielu, IFACE), ZGRUND(ielu), STR(0), W, XAFULL(LINK), LINK, &
                             COCBCD(1, IBC), GETHRF(ielu), HOCNOW(ICAT), QOCF(ICAT), QJ(0), DQ(0, 0))
                  
                  DQ(0, 1) = zero
                  CALL SETQSA(ielu, IFACE, QJ(0))  ! -------- STORE FLUXES IN GLOBAL ARRAYS
                  DQ0ST(ielu, IFACE) = DQ(0, 0)
                  DQIST(ielu, IFACE) = DQ(0, 1)
               END IF
               
            ELSE IF (SINGLE) THEN
               JMAX = 1
               JEL2(1) = JEL
               JFACE2(1) = ICMREF(ielu, IFACE + 8)
               JEL2(0) = ielu
               JFACE2(0) = IFACE
               
               single_data_loop: DO J = 0, JMAX
                  KEL = JEL2(J)
                  IF (KEL < 1) CYCLE single_data_loop
                  
                  KFACE = JFACE2(J)
                  ZI(J) = GETHRF(KEL)
                  LI(J) = DHF(KEL, KFACE)
                  ZGI(J) = ZGRUND(KEL)
                  STR(J) = FSTR(KEL, KFACE)
                  
                  ! surface storage (sb 1905022)
                  IF (STRXX(KEL) < 0.0d0) THEN
                     IF ((GETHRF(KEL) - ZGRUND(KEL)) < (-STRXX(KEL) / 1000.0d0)) THEN
                        STR(J) = 0.5d0
                     ELSE
                        STR(J) = 2.0d0
                     END IF
                  END IF

                  IF (KEL > total_no_links) CYCLE single_data_loop
                  CW(J) = CWIDTH(KEL)
                  XA(J) = XAFULL(KEL)
                  jxswork(J) = KEL
               END DO single_data_loop
               
               JFACE = JFACE2(1)
               IF (ielu <= total_no_links .AND. JEL > total_no_links) THEN  ! * link-land
                  LI(0) = zero
                  ZGI(0) = ZBFULL(ielu)
                  ! PERF FIX: Pass full array names instead of (0:1) slices
                  CALL OCQBNK(CLENTH(ielu), LI, ZGI, STR, ZI, QJ, DQ)
               ELSE
                  ! * test for internal boundary
                  JBC = NOCBCC(JEL)
                  NBC = 0
                  IF (IFACE == NFACE) THEN
                     NBC = IBC
                  ELSE IF (JBC > 0) THEN
                     IF (JFACE == NOCBCD(JBC, 2)) NBC = JBC
                  END IF
                  
                  NTYPE = 0
                  IF (NBC > 0) NTYPE = NOCBCD(NBC, 3)
                  
                  ! * land-land or link-link
                  IF (ielu > total_no_links) THEN
                     W = FDQQ(ielu, IFACE)
                     ! PERF FIX: Pass full array names instead of (0:1) slices
                     CALL OCQGRD(NTYPE, LI, ZGI, STR, W, ZI, QJ, DQ)
                  ELSE
                     itemp = MAX(1, NBC)
                     
                     !***ZQ Module 200520
                     DO i = 1, NoZQTables
                        IF (((ielu == ZQTableLink(i)) .AND. (IFACE == ZQTableFace(i))) .OR. &
                            ((JEL == ZQTableLink(i)) .AND. (JFACE == ZQTableFace(i)))) THEN
                           ZQTableRef = i
                           NTYPE = 12
                        END IF
                     END DO
                     !!***ZQ Module 200520 end

                     ! PERF FIX: Pass full arrays and base address COCBCD(1, itemp) instead of slices
                     CALL OCQLNK(NTYPE, LI, ZGI, STR, CW, XA, &
                                 jxswork, COCBCD(1, itemp), ZI, QJ, DQ)
                  END IF
               END IF
               
               CALL SETQSA(JEL, JFACE, QJ(1))
               DQ0ST(JEL, JFACE) = DQ(1, 1)
               DQIST(JEL, JFACE) = DQ(1, 0)
               
               CALL SETQSA(ielu, IFACE, QJ(0))
               DQ0ST(ielu, IFACE) = DQ(0, 0)
               DQIST(ielu, IFACE) = DQ(0, 1)
               
            ELSE IF (MULTI) THEN
               JMAX = 3
               IBR = -JEL
               
               multi_setup_loop: DO J = 1, JMAX
                  KEL = ICMRF2(IBR, J)
                  IF (KEL > 0) THEN
                     ! Directly cycle the outer face loop to skip processing this face
                     IF (KEL < ielu) CYCLE face_loop
                     JFACE2(J) = ICMRF2(IBR, J + 3)
                  END IF
                  JEL2(J) = KEL
               END DO multi_setup_loop
               
               JEL2(0) = ielu
               JFACE2(0) = IFACE
               
               multi_data_loop: DO J = 0, JMAX
                  KEL = JEL2(J)
                  IF (KEL < 1) CYCLE multi_data_loop
                  
                  KFACE = JFACE2(J)
                  ZI(J) = GETHRF(KEL)
                  LI(J) = DHF(KEL, KFACE)
                  ZGI(J) = ZGRUND(KEL)
                  STR(J) = FSTR(KEL, KFACE)
                  
                  ! surface storage (sb 1905022)
                  IF (STRXX(KEL) < 0.0d0) THEN
                     IF ((GETHRF(KEL) - ZGRUND(KEL)) < (-STRXX(KEL) / 1000.0d0)) THEN
                        STR(J) = 0.5d0
                     ELSE
                        STR(J) = 2.0d0
                     END IF
                  END IF

                  IF (KEL > total_no_links) CYCLE multi_data_loop
                  CW(J) = CWIDTH(KEL)
                  XA(J) = XAFULL(KEL)
                  jxswork(J) = KEL
               END DO multi_data_loop
               
               ! PERF FIX: Full arrays passed without slices
               CALL OCQMLN(ielu, JEL2, LI, ZGI, STR, CW, XA, ZI, QJ, DQIJ, jxswork)
               
               multi_scatter_loop: DO J = 0, JMAX
                  KEL = JEL2(J)
                  IF (KEL == 0) CYCLE multi_scatter_loop
                  
                  KFACE = JFACE2(J)
                  CALL SETQSA(KEL, KFACE, QJ(J))
                  DQ0ST(KEL, KFACE) = DQIJ(J, J)
                  
                  IF (J > 0) THEN
                     DQIST2(IBR, J) = DQIJ(0, J)
                     JBR = -ICMREF(KEL, KFACE + 4)
                     
                     DO JJ = 1, 3
                        KEL = ICMRF2(JBR, JJ)
                        IF (KEL > 0) THEN
                           JJJ = MOD(J + JJ, 4)
                           DQIST2(JBR, JJ) = DQIJ(J, JJJ)
                        END IF
                     END DO
                  END IF
               END DO multi_scatter_loop
               
            END IF
         END DO face_loop
      END DO element_loop

   END SUBROUTINE OCQDQ


!FFFFFF FUNCTION fstr
   PURE FUNCTION fstr(jel,face) RESULT(r)
      INTEGER, INTENT(IN) :: jel, face
      DOUBLEPRECISION     :: r
!mult = DBLE(MOD(face, 2))
!r    = mult * strxx(jel) + (one-mult) * stryy(jel)
      IF(face==1 .OR. face==3) THEN
         r = strxx(jel)
      ELSE
         r = stryy(jel)
      ENDIF
   END FUNCTION fstr


!FFFFFF FUNCTION fdqq
   PURE FUNCTION fdqq(jel, face) RESULT(r)
      INTEGER, INTENT(IN) :: jel, face
      DOUBLEPRECISION     :: r
!mult = DBLE(MOD(face,2))
!r    = mult * dyqq(jel) + (one-mult) * dxqq(jel)
      IF(face==1 .OR. face==3) THEN
         r = dyqq(jel)
      ELSE
         r = dxqq(jel)
      ENDIF
   END FUNCTION fdqq

END MODULE ocqdqmod
