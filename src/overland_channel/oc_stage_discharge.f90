!> summary: The stage--discharge derivatives assembled for the implicit solve.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[OCQDQ]] visits every active face, calls the matching routine in
!> [[oc_discharge]], and stores the discharge together with its derivatives
!> with respect to the local and adjacent water levels in [[oc_state]]'s
!> `DQ0ST`, `DQIST` and `DQIST2`. `fstr` and `fdqq` are its two helpers.
!>
!> Those derivatives are what makes the overland/channel step implicit, which
!> is why this module sits between the per-face discharge formulae and the
!> driver rather than inside either.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of OCQDQMOD; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_stage_discharge

   USE MOD_PARAMETERS, ONLY: zero
   USE element_geometry, ONLY: DHF, DXQQ, DYQQ, total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF, ICMRF2
   USE channel_geometry, ONLY: CLENTH, CWIDTH, ZBFULL
   USE oc_state, ONLY: DQ0ST, DQIST, DQIST2, STRXX, STRYY
   USE oc_boundaries, ONLY: COCBCD, HOCNOW, NOCBCC, NOCBCD, QOCF
   USE oc_cross_sections, ONLY: XAFULL
   USE oc_node_solver, ONLY: gethrf, setqsa
   USE oc_discharge, ONLY: OCQBC, OCQBNK, OCQGRD, OCQLNK, OCQMLN
   USE zq_tables, ONLY: NoZQTables, ZQTableFace, ZQTableLink, ZQTableRef

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: OCQDQ

CONTAINS

   !> Calculates overland/channel face flows and flow derivatives.
   !>
   !> The routine loops over every element face (`element_loop`/`face_loop`),
   !> dispatching to the appropriate hydraulic calculation for external
   !> boundary conditions, land-land faces, link-link faces, link-bank faces,
   !> and multi-link junctions. It scatters resulting flows into the global
   !> face-flow arrays and stores derivative terms for the solver. A face
   !> already handled from its lower-numbered neighbour is skipped: for an
   !> ordinary single neighbour, `SINGLE .AND. JEL < ielu` cycles `face_loop`;
   !> for a multi-link branch, `multi_setup_loop` cycles `face_loop` directly
   !> as soon as any active participant element number is below `ielu`, so the
   !> junction is instead processed once, from its lowest-numbered member.
   !>
   !> For a single face between local side `0` and neighbour side `1`, the
   !> called hydraulic helper returns
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
   !> For a multi-link junction, [[oc_discharge:OCQMLN]] returns branch flows
   !> \(Q_j\) and the derivative matrix \(DQIJ(j,k)=\partial Q_j/\partial
   !> Z_k\). The diagonal terms are stored in `DQ0ST`, while off-diagonal
   !> confluence couplings are stored in `DQIST2`. In ordinary sign
   !> conventions the self derivative is usually negative (`DQ0ST < 0`) and
   !> neighbour derivatives are usually positive (`DQIST`/`DQIST2 > 0`), but
   !> dry states, boundary controls, or limiting can alter those values.
   !>
   !> Surface storage: for single- and multi-link participants, a negative
   !> `STRXX(kel)` is treated as a millimetre-scale ponding-depth marker
   !> rather than roughness (see the module-level table); the substituted
   !> value feeds [[oc_discharge:OCQBNK]], [[oc_discharge:OCQGRD]], [[oc_discharge:OCQLNK]], or
   !> [[oc_discharge:OCQMLN]] in place of [[fstr]]'s directional roughness.
   !>
   !> If either side of a link-link face matches a configured ZQ table
   !> (`ZQTableLink`, `ZQTableFace`), `OCQDQ` sets `ZQTableRef` and dispatches
   !> the face as boundary type `12`, so [[oc_discharge:OCQLNK]] obtains discharge
   !> from the ZQ rating table instead of the ordinary link-link equation.
   !>
   !> Entry requirements retained from the legacy routine are:
   !>
   !> | Requirement | Meaning |
   !> |:------------|:--------|
   !> | `NELEE >= total_no_elements`, `total_no_elements >= 1`, `NXSCEE >= 1` | Active elements and cross-section tables fit the compiled extents. |
   !> | `NLFEE >= 1` and `NLFEE >= -ICMREF(1:total_no_elements,5:8)` for negative face references | Link/confluence references fit the link extent. |
   !> | `NOCTAB >= 1` and `NOCTAB >= NOCBCC(1:total_no_elements)` | Boundary-condition indices fit the boundary table. |
   !> | For each negative face reference `i`, `ICMRF2(-i,1:3) <= total_no_elements` | Multi-link participant elements fit the element extent. |
   !> | For each negative face reference `i`, `1 <= ICMRF2(-i,4:6) <= 4` | Multi-link participant face numbers are valid. |
   !> | For each external boundary `ibc=NOCBCC(iel)>0`, `NOCBCD(ibc,2)` identifies a face whose `ICMREF` neighbour is external or component-compatible | Boundary metadata is consistent with the element topology. |
   !>
   !> Boundary conflicts where both sides of a face carry non-zero boundary
   !> condition indices are disallowed. The routine also assumes the
   !> consistency between `ICMREF` and `ICMRF2` checked by the multi-link
   !> scatter loop (`multi_scatter_loop`).
   !>
   !> @note
   !> This routine has no dummy arguments. It uses shared grid,
   !> boundary, geometry, water-level, and ZQ-table state from the `core/`
   !> modules, [[oc_boundaries]], [[oc_state]] and [[zq_tables]]. Several calls
   !> into [[oc_discharge]]
   !> pass whole local work arrays (declared `(0:3)`) to dummy arguments
   !> declared `(0:1)`, and pass single-element addresses
   !> (`COCBCD(1,ibc)`/`COCBCD(1,itemp)`) to array dummy arguments, relying on
   !> standard Fortran sequence association through the explicit-shape actual
   !> arguments rather than array-section copies.
   !> @endnote
   SUBROUTINE OCQDQ ()

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
                     
                     ! ZQ Module 200520: override with a configured ZQ rating table, if this face has one
                     DO i = 1, NoZQTables
                        IF (((ielu == ZQTableLink(i)) .AND. (IFACE == ZQTableFace(i))) .OR. &
                            ((JEL == ZQTableLink(i)) .AND. (JFACE == ZQTableFace(i)))) THEN
                           ZQTableRef = i
                           NTYPE = 12
                        END IF
                     END DO
                     ! ZQ Module 200520 end

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

   !> Returns the Strickler/roughness value for a face direction.
   !>
   !> Faces 1 and 3 use `STRXX`; faces 2 and 4 use `STRYY`.
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 2026-04-09 | SvB | 4.6.1 | Declared `PURE` (commit `738cc38`). |
   !> @endhistory
   PURE FUNCTION fstr(jel,face) RESULT(r)
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
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 2026-04-09 | SvB | 4.6.1 | Declared `PURE` (commit `738cc38`). |
   !> @endhistory
   PURE FUNCTION fdqq(jel, face) RESULT(r)
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

END MODULE oc_stage_discharge

