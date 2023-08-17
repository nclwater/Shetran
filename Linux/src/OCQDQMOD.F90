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
INTEGER                         :: i
INTEGER                         :: jxswork(0:3)
INTEGER                         :: IBC, IBR, IELu, IFACE, ICAT, NBC, NTYPE, NFACE  
INTEGER                         :: JBC, JBR, JEL, JFACE, J, JJ, JJJ, JMAX, KEL, KFACE, LINK, itemp
INTEGER, DIMENSION(0:3)         :: JEL2, JFACE2
DOUBLEPRECISION, DIMENSION(0:3) :: CW, LI, STR, QJ, XA, ZI, ZGI
DOUBLEPRECISION                 :: DQ(0:1,0:1), DQIJ(0:3,0:3)
DOUBLEPRECISION                 :: W
LOGICAL                         :: MULTI, SINGLE, cycle500, eexternal
 QJ   = zero  
 DQ  = zero  
out600 : DO ielu = 1, total_no_elements  
    IBC   = NOCBCC(ielu)  ! ----- BC index and face number
    IF (IBC.GT.0) THEN
        NFACE = NOCBCD(IBC,2)
    ELSE
        NFACE = 0
    ENDIF 
    OUT500 : DO IFACE = 1, 4
        cycle500 = .FALSE. !AD needs this
        JEL       = ICMREF(ielu,IFACE+4)  
        SINGLE    = JEL.GT.0
        IF ((JEL.LT.ielu).AND.SINGLE) CYCLE out500 !GOTO 500   !>>>>>>>>
        MULTI     = JEL.LT.0
        eexternal = JEL==0
        IF(eexternal) THEN
            IF(NFACE.EQ.IFACE) THEN  
                W = FDQQ(ielu,IFACE)  
                STR (0) = FSTR (ielu, IFACE)  
                NTYPE = NOCBCD (IBC, 3)  
                ICAT = NOCBCD (IBC, 4)  
                LINK = MAX (1, MIN (ielu, total_no_links) )  
                CALL OCQBC (NTYPE, DHF(ielu,IFACE), ZGRUND(ielu), STR(0), W, XAFULL(LINK), LINK, &
                            COCBCD(1:5,IBC), GETHRF(ielu), HOCNOW(ICAT), QOCF(ICAT), QJ(0), DQ(0,0) )
                            DQ (0, 1) = zero  
                CALL SETQSA(ielu, IFACE, QJ(0))  ! -------- STORE FLUXES IN GLOBAL ARRAYS
                DQ0ST (ielu, IFACE) = DQ (0, 0)  
                DQIST (ielu, IFACE) = DQ (0, 1)  
            ENDIF
       ELSEIF(single) THEN
            JMAX = 1  
            JEL2 (1) = JEL  
            JFACE2 (1) = ICMREF (ielu, IFACE+8)  
            JEL2 (0) = ielu  
            JFACE2 (0) = IFACE  
            out110 : DO J = 0, JMAX  !               * Use the lists to gather the data
                KEL = JEL2 (J)  
                IF (KEL.LT.1) CYCLE out110 !GOTO 160  
                KFACE = JFACE2 (J)  
                ZI (J) = GETHRF (KEL)  
                LI (J) = DHF (KEL, KFACE)  
                ZGI (J) = ZGRUND (KEL)  
                STR (J) = FSTR (KEL, KFACE)  
!!! surface storage
!!! sb 1905022
                if (STRXX(kel).lt.0) then
                    if ((gethrf(kel)-zgrund(kel)).lt.(-STRXX(kel)/1000.0)) then
                       str(j)=0.5
                    else 
                       zi(j) = GETHRF(KEL)+0.95*strxx(kel)/1000
                       str(j)=2.0
                    endif
!                    write(582,*),kel,j,gethrf(kel)-zgrund(kel),zi(j),zgi(j),str(j)
                endif

                IF (KEL.GT.total_no_links) CYCLE out110 !GOTO 160  
                CW (J) = CWIDTH (KEL)  
                XA (J) = XAFULL (KEL)
                jxswork(j) = kel
            ENDDO out110
            JFACE = JFACE2 (1)  
            IF ((ielu.LE.total_no_links).AND.(JEL.GT.total_no_links)) THEN  !                   * link-land
                LI (0) = zero  
                ZGI (0) = ZBFULL (ielu)  
                CALL OCQBNK (CLENTH (ielu), LI(0:1), ZGI(0:1), STR(0:1), ZI(0:1), QJ(0:1), DQ)  
            ELSE  
                !                   * test for internal boundary
                JBC = NOCBCC (JEL)  
                NBC = 0  
                IF (IFACE.EQ.NFACE) THEN  
                    NBC = IBC  
                ELSEIF (JBC.GT.0) THEN  
                    IF (JFACE.EQ.NOCBCD (JBC, 2) ) NBC = JBC  
                ENDIF  
                NTYPE = 0  
                IF (NBC.GT.0) NTYPE = NOCBCD (NBC, 3)  
                !                   * land-land or link-link
                IF (ielu.GT.total_no_links) THEN  
                    W = FDQQ (ielu,IFACE)  
                    CALL OCQGRD (NTYPE, LI(0:1), ZGI(0:1), STR(0:1), W, ZI(0:1), QJ(0:1), DQ)  
                ELSE
                    itemp = MAX(1,NBC)
!***ZQ Module 200520
                    do i=1,NoZQTables
                    if (((ielu.eq.ZQTableLink(i)).and.(iface.eq.ZQTableFace(i))).or.((jel.eq.ZQTableLink(i)).and.(jface.eq.ZQTableFace(i)))) then
                       ZQTableRef=i
                       ntype=12
                    endif
                    enddo
 !!***ZQ Module 200520 end

                    CALL OCQLNK (NTYPE, LI(0:1), ZGI(0:1), STR(0:1), CW(0:1), XA(0:1), &
                                 jXSwork, COCBCD(1:3,itemp), ZI(0:1), QJ(0:1), DQ)
                ENDIF  
            ENDIF  
            CALL SETQSA(JEL, JFACE, QJ(1))  
            DQ0ST (JEL, JFACE) = DQ (1, 1)  
            DQIST (JEL, JFACE) = DQ (1, 0)
            CALL SETQSA(ielu, IFACE, QJ(0))  
            DQ0ST (ielu, IFACE) = DQ (0, 0)  
            DQIST (ielu, IFACE) = DQ (0, 1)
        ELSEIF(multi) THEN  
            JMAX = 3  
            IBR = - JEL
            out100 : DO J = 1, JMAX
                IF(cycle500) CYCLE out100
                KEL = ICMRF2 (IBR, J)  
                IF (KEL.GT.0) THEN  
                    IF (KEL.LT.ielu) THEN  !GOTO 500  !>>>>>>>>
                        cycle500=.TRUE. 
                        CYCLE out100
                    ENDIF
                    JFACE2 (J) = ICMRF2 (IBR, J + 3)  
                ENDIF  
                JEL2 (J) = KEL  
            ENDDO out100
            IF(cycle500) CYCLE out500
            JEL2 (0) = ielu  
            JFACE2 (0) = IFACE  
            out160 : DO J = 0, JMAX  !               * Use the lists to gather the data
                KEL = JEL2 (J)  
                IF (KEL.LT.1) CYCLE out160 !GOTO 160  
                KFACE = JFACE2 (J)  
                ZI (J) = GETHRF (KEL)  
                LI (J) = DHF (KEL, KFACE)  
                ZGI (J) = ZGRUND (KEL)  
                STR (J) = FSTR (KEL, KFACE)  
!!! surface storage
!!! sb 1905022
                if (STRXX(kel).lt.0) then
                    if ((gethrf(kel)-zgrund(kel)).lt.(-STRXX(kel)/1000.0)) then
                       str(j)=0.5
                    else 
                       zi(j) = GETHRF(KEL)+strxx(kel)/1000
                       str(j)=2.0
                    endif
!                    write(582,*),kel,j,gethrf(kel)-zgrund(kel),zi(j),zgi(j),str(j)
                endif

                IF (KEL.GT.total_no_links) CYCLE out160 !GOTO 160  
                CW (J) = CWIDTH (KEL)  
                XA (J) = XAFULL (KEL)
                jxswork(j) = kel
            ENDDO out160
            !               * Calculate flows & derivatives, and scatter
            CALL OCQMLN (ielu, JEL2, LI, ZGI, STR, CW, XA, ZI, QJ, DQIJ, jXSwork)
            out260 : DO J = 0, JMAX  
                KEL = JEL2 (J)  
                IF (KEL.EQ.0) CYCLE out260 !GOTO 260  
                KFACE = JFACE2 (J)  
                CALL SETQSA(KEL, KFACE,QJ(J))
                DQ0ST (KEL, KFACE) = DQIJ (J, J)  
                IF (J.GT.0) THEN  
                    DQIST2 (IBR, J) = DQIJ (0, J)  
                    JBR = - ICMREF (KEL, KFACE+4)  
                    DO JJ = 1, 3  !240
                        KEL = ICMRF2 (JBR, JJ)  
                        IF (KEL.GT.0) THEN  
                            JJJ = MOD (J + JJ, 4)  
                            DQIST2 (JBR, JJ) = DQIJ (J, JJJ)  
                        ENDIF  
                    ENDDO !240  
                ENDIF  
            ENDDO out260
        ENDIF
    ENDDO out500
 ENDDO out600
END SUBROUTINE OCQDQ

 
!FFFFFF FUNCTION fstr
FUNCTION fstr(jel,face) RESULT(r)
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
FUNCTION fdqq(jel, face) RESULT(r)
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
