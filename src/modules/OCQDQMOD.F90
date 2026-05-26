!> summary: Overland/channel face flow and derivative controller.
!> author: JE, Newcastle University; RAH, Newcastle University; SB, Newcastle University
!>
!> `OCQDQMOD` controls calculation of overland and channel flows, together with
!> derivatives used by the overland/channel solver, at element faces. It handles
!> external boundaries, single adjacent faces, multi-way branch faces, bank
!> exchanges, land-grid exchanges, link-link exchanges, and ZQ reservoir-table
!> routing hooks.
!>
!> `STRXX` and `STRYY` normally hold directional Strickler roughness values.
!> For land or link participants passed through `OCQDQ`, a negative `STRXX`
!> is used as a surface-storage marker rather than as a physical roughness:
!>
!> | Condition | Effective roughness passed to flow helper |
!> |:----------|:------------------------------------------|
!> | `STRXX(kel) >= 0` | Directional value from [[fstr]] |
!> | `STRXX(kel) < 0` and `HRF-ZGRUND < -STRXX/1000` | `0.5` |
!> | `STRXX(kel) < 0` and `HRF-ZGRUND >= -STRXX/1000` | `2.0` |
!>
!> The threshold depth is therefore stored in millimetres as `-STRXX`; the
!> active hydraulic calculation receives the fixed effective values above.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought implicit declarations from `SPEC.AL`. |
!> | 1998-02-24 | RAH | 4.2 | Reworked face arguments and loop structure; added explicit typing. |
!> | 1998-02-25 | RAH | 4.2 | Called face-flow routines on lowest element and restructured boundary handling. |
!> | 1998-02-26 | RAH | 4.2 | Replaced multi-call interface with one element loop. |
!> | 1998-03-27 | RAH | 4.2 | Added `XAFULL` input argument. |
!> | 1998-03-31 | RAH | 4.2 | Reworked `OCQGRD` arguments and derivative arrays. |
!> | 1998-04 | RAH | 4.2 | Reworked bank, link, and boundary-condition calls. |
!> | 1998-08-07 | RAH | 4.2 | Added local `LINK` to avoid out-of-bounds access. |
!> | 2009-01 | JE | 4.3.5F90 | Converted to Fortran 90. |
!> | 2019-05-22 | SB | - | Added negative-`STRXX` surface-storage switching. |
!> | 2020-05-20 | SB | - | Added ZQ table routing support. |
!> @endhistory
MODULE ocqdqmod
USE SGLOBAL
USE AL_C ,     ONLY : ICMRF2, CWIDTH, DHF, ZBFULL, CLENTH
USE AL_G ,     ONLY : ICMREF, ICMXY
USE AL_D ,     ONLY : DQ0ST, DQIST, DQIST2, NOCBCC, NOCBCD, NoZQTables,ZQTableRef, ZQTableLink,ZQTableFace
USE OCmod2 ,   ONLY : GETHRF, OCQMLN, SETQSA, OCQBNK, OCQGRD, OCQLNK, OCQBC

IMPLICIT NONE
DOUBLEPRECISION    :: XAFULL(NLFEE)       !! Full-flow cross-sectional area for each channel link.
DOUBLEPRECISION    :: COCBCD(5, NOCTAB)   !! Real-valued overland/channel boundary-condition coefficients.
DOUBLEPRECISION    :: HOCNOW(NOCTAB)      !! Current boundary stage/head values by boundary category.
DOUBLEPRECISION    :: QOCF(NOCTAB)        !! Current prescribed overland/channel boundary flow values by category.
DOUBLEPRECISION    :: STRXX(NELEE)        !! X-direction Strickler roughness, or negative storage-depth marker.
DOUBLEPRECISION    :: STRYY(NELEE)        !! Y-direction Strickler roughness.
!LOGICAL            :: firstocqdq=.TRUE.


PRIVATE
PUBLIC :: OCQDQ, STRXX, STRYY, HOCNOW, QOCF, XAFULL, COCBCD ! , firstocqdq

CONTAINS

!> Calculates overland/channel face flows and flow derivatives.
!>
!> The routine loops over every element face, dispatching to the appropriate
!> hydraulic calculation for external boundary conditions, land-land faces,
!> link-link faces, link-bank faces, and multi-link junctions. It scatters
!> resulting flows into the global face-flow arrays and stores derivative terms
!> for the solver.
!>
!> For a single face between local side `0` and neighbour side `1`, the called
!> hydraulic helper returns
!>
!> \[
!> Q_j = Q_j(Z_0,Z_1),\qquad
!> DQ(j,k) = \frac{\partial Q_j}{\partial Z_k},
!> \quad j,k\in\{0,1\}.
!> \]
!>
!> `OCQDQ` writes these to the global arrays as
!>
!> \[
!> QSA(iel,iface)=Q_0,\quad DQ0ST(iel,iface)=DQ(0,0),
!> \quad DQIST(iel,iface)=DQ(0,1),
!> \]
!>
!> and, where there is a regular neighbour,
!>
!> \[
!> QSA(jel,jface)=Q_1,\quad DQ0ST(jel,jface)=DQ(1,1),
!> \quad DQIST(jel,jface)=DQ(1,0).
!> \]
!>
!> For a multi-link junction, [[ocqmln]] returns branch flows \(Q_j\) and the
!> derivative matrix \(DQIJ(j,k)=\partial Q_j/\partial Z_k\). The diagonal
!> terms are stored in `DQ0ST`, while off-diagonal confluence couplings are
!> stored in `DQIST2`. In ordinary sign conventions the self derivative is
!> usually negative (`DQ0ST < 0`) and neighbour derivatives are usually positive
!> (`DQIST`/`DQIST2 > 0`), but dry states, boundary controls, or limiting can
!> alter those values.
!>
!> If either side of a link-link face matches a configured ZQ table
!> (`ZQTableLink`, `ZQTableFace`), `OCQDQ` sets `ZQTableRef` and dispatches the
!> face as boundary type `12`, so [[ocqlnk]] obtains discharge from the ZQ
!> rating table instead of the ordinary link-link equation.
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NELEE >= NEL`, `NEL >= 1`, `NXSCEE >= 1` | Active elements and cross-section tables fit the compiled extents. |
!> | `NLFEE >= 1` and `NLFEE >= -ICMREF(1:NEL,5:8)` for negative face references | Link/confluence references fit the link extent. |
!> | `NOCTAB >= 1` and `NOCTAB >= NOCBCC(1:NEL)` | Boundary-condition indices fit the boundary table. |
!> | For each negative face reference `i`, `ICMRF2(-i,1:3) <= NEL` | Multi-link participant elements fit the element extent. |
!> | For each negative face reference `i`, `1 <= ICMRF2(-i,4:6) <= 4` | Multi-link participant face numbers are valid. |
!> | For each external boundary `ibc=NOCBCC(iel)>0`, `NOCBCD(ibc,2)` identifies a face whose `ICMREF` neighbour is external or component-compatible | Boundary metadata is consistent with the element topology. |
!>
!> Boundary conflicts where both sides of a face carry non-zero boundary
!> condition indices are disallowed. The routine also assumes the consistency
!> between `ICMREF` and `ICMRF2` checked by the multi-link scatter loop.
!>
!> @note This routine has no dummy arguments. It uses shared grid, boundary,
!> geometry, water-level, and ZQ-table state from `SGLOBAL`, `AL_C`, `AL_D`,
!> `AL_G`, and `OCmod2`.
!> @endnote
SUBROUTINE OCQDQ ()
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
 !!                      zi(j) = GETHRF(KEL)+0.95*strxx(kel)/1000
                       str(j)=2.0
                    endif
!!                    if (kel.eq.36) then
!!                      write(582,*) j,gethrf(kel)-zgrund(kel),str(j)
!!                    endif
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
!!                       zi(j) = GETHRF(KEL)+0.95*strxx(kel)/1000
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



!> Returns the Strickler/roughness value for a face direction.
!>
!> Faces 1 and 3 use `STRXX`; faces 2 and 4 use `STRYY`.
FUNCTION fstr(jel,face) RESULT(r)
INTEGER, INTENT(IN) :: jel  !! Element index.
INTEGER, INTENT(IN) :: face !! Face number.
DOUBLEPRECISION     :: r    !! Roughness/Strickler value for the requested face.
!mult = DBLE(MOD(face, 2))
!r    = mult * strxx(jel) + (one-mult) * stryy(jel)
IF(face==1 .OR. face==3) THEN
    r = strxx(jel)
ELSE
    r = stryy(jel)
ENDIF
END FUNCTION fstr



!> Returns the transverse face length used in a face-flow calculation.
!>
!> Faces 1 and 3 use `DYQQ`; faces 2 and 4 use `DXQQ`.
FUNCTION fdqq(jel, face) RESULT(r)
INTEGER, INTENT(IN) :: jel  !! Element index.
INTEGER, INTENT(IN) :: face !! Face number.
DOUBLEPRECISION     :: r    !! Transverse face length associated with the requested face.
!mult = DBLE(MOD(face,2))
!r    = mult * dyqq(jel) + (one-mult) * dxqq(jel)
IF(face==1 .OR. face==3) THEN
    r = dyqq(jel)
ELSE
    r = dxqq(jel)
ENDIF
END FUNCTION fdqq

END MODULE ocqdqmod
