!> summary: Well, spring, boundary and interception source terms for a column.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> Six routines that each contribute one source or sink term to the column
!> problem [[vs_column_solver:VSCOLM]] assembles: [[VSWELL]] pumped
!> abstraction, [[VSSPR]] spring discharge, [[VSINTC]] the interception and
!> surface exchange at the top of the column, [[VSLOWR]] and [[VSUPPR]] the
!> lower and upper boundary inflows, and [[VSSAI]] the lateral inflow.
!>
!> They take everything through arguments, which is why this module imports
!> almost nothing and can sit below the solver in the dependency order.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995--1998 | GP / RAH | 4.0--4.2 | Created the VSS component and its `.INC` include groups. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the VSS Fortran sources into a single Fortran 90 module. |
!> | 2026-03 to 2026-05 | SB / SvB | 4.6 | Modernisation pass, and moved `VSREAD`'s read buffers to allocatable module state to avoid a stack-related crash. |
!> | 2026-09-10 | SvB | - | Split out of VSmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE vs_sources

   USE MOD_PARAMETERS, ONLY: half, one, zero
   USE float_compare, ONLY: gezero, gtzero

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: VSINTC, VSLOWR, VSSAI, VSSPR, VSUPPR, VSWELL

CONTAINS

!> Adds inter-column exchange coefficients to the column system.
!>
!> `VSINTC` assembles the base tridiagonal system for one VSS column before
!> [[vscolm]] adds upper, lower, well, spring, lateral-boundary, and
!> stream-aquifer terms. It combines storage, vertical inter-cell flow, internal
!> lateral exchange to already known neighbour heads, and existing source/sink
!> terms `CQ`.
!>
!> Required entry conditions are `1 <= ICBOT <= ICTOP <= LLEE` and `DT > 0`. In
!> addition, for every face `j` with `JELDUM(j) > 0` and `JCBC(j) /= 9`, and
!> every cell `i` with `JCACN(j,i) /= 0`, both `k = JCACN(j,i)` and
!> `k1 = k + JCDEL1(k,j)` must lie in `1:LLEE`.
!> For any face with a regular neighbour (`JELDUM(j)>0`) that is not handled as
!> stream-aquifer interaction (`JCBC(j) /= 9`), each non-zero `JCACN(j,i)` must
!> point to a valid neighbour cell `k`, and `k1 = k + JCDEL1(k,j)` must also be
!> valid. The neighbour heads and conductances supplied by [[vssim]] and
!> [[vscoef]] are assumed to be consistent with those indices.
!>
!> The scheme is currently fully implicit (`SIGMA = 1`). Effective hydraulic
!> head is formed as
!>
!> \[
!>   H_i = \sigma\psi_i + (1-\sigma)\psi_i^n + z_i,\qquad \sigma=1.
!> \]
!>
!> For each cell, the storage volume factor and linearised storage terms are
!>
!> \[
!>   V_i/\Delta t = {CDELZ_i\,CA0\over DT},\qquad
!>   G_i = CETA_i\,V_i/\Delta t,\qquad
!>   G'_i = CDETA_i\,V_i/\Delta t .
!> \]
!>
!> Using the vertical conductances `CBETM(i)` and `CBETM(i+1)` from [[vscoef]],
!> the routine fills lower diagonal `CA`, upper diagonal `CC`, diagonal `CB`,
!> and right-hand side `CR`. In compact form, the residual being linearised is
!>
!> \[
!>   R_i =
!>   H_{i-1}\beta_i - H_i CF_i + H_{i+1}\beta_{i+1}
!>   -(\psi_i-\psi_i^n)G_i + CQ_i ,
!> \]
!>
!> with derivative terms from `CDBETM`, `CDBTMM`, and `CDF` included in the
!> assembled matrix.
!>
!> Lateral neighbour contributions are then added for regular faces:
!>
!> \[
!>   CR_i \leftarrow CR_i - H_k\gamma_1 - H_{k1}\gamma_2,\qquad
!>   CB_i \leftarrow CB_i + H_k\gamma'_1 + H_{k1}\gamma'_2,
!> \]
!>
!> where `CGAM1/2` and `CDGAM1/2` are lateral conductances and derivatives from
!> [[vscoef]]. Faces with `JCBC=9` are skipped here because [[vssai]] adds those
!> stream-aquifer terms separately. Faces with `JCBC=10` are not skipped by this
!> routine; any non-zero `JCACN` entries still contribute regular lateral terms,
!> and [[vssai]] then adds the stream-aquifer contribution.
!>
!> @note
!> `CQ` is already premultiplied by the cell volume factor in [[vssim]], as of
!> the 1997-05-14 change recorded below. This routine treats it as an assembled
!> residual/source term, not as a flux density to be scaled again.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-20 | GP | 4.0 | Written; version 4.0 completed 1995-06-22. |
!> | 1997-01-20 | RAH | 4.1 | Rewritten to use fewer operations and to stop overwriting the input arrays. |
!> | 1997-01-26 | RAH | 4.1 | Dispensed with the inputs `IEL`, `CB*P`, `CD*P`, `CDFM`, and `C*G`; passed data through arguments instead of `COMMON`. |
!> | 1997-02-03 | RAH | 4.1 | Replaced input `CV` with `CA0` and `CDELZ`. |
!> | 1997-02-10 | RAH | 4.1 | Made the input `SIGMA` a local. |
!> | 1997-05-14 | RAH | 4.1 | `CQ` is now pre-multiplied by `CA0*CDELZ` by the caller (see [[vssim]]); swapped the `JCACN` indices. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote the lateral-terms `GOTO`-skip logic as `CYCLE` on named loops; added the `PURE` attribute (the routine performs no I/O and modifies only its `INTENT(OUT)` dummy arguments). No change to the assembled coefficients. |
!> @endhistory
   PURE SUBROUTINE VSINTC(LLEE, ICBOT, ICTOP, JELDUM, JCBC, JCACN, &
                          JCDEL1, CA0, CDELZ, CZ, CZ1, DT, CETA, CDETA, CQ, CPSI, CPSIN, CF, &
                          CDF, CBETM, CDBETM, CDBTMM, CPSI1, CPSIN1, CGAM1, CGAM2, CDGAM1, &
                          CDGAM2, CA, CB, CC, CR, H)

      ! Assumed external module dependencies providing global variables:
      ! zero

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE                  !! Declared cell dimension for neighbour arrays.
      INTEGER, INTENT(IN) :: ICBOT                 !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                 !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: JELDUM(4)             !! Adjacent element id by face; values below 1 disable regular lateral coupling.
      INTEGER, INTENT(IN) :: JCBC(4)               !! Boundary type by face; type 9 is skipped here.
      INTEGER, INTENT(IN) :: JCACN(4, ICBOT:ICTOP)  !! Adjacent-cell index by face and active cell.
      INTEGER, INTENT(IN) :: JCDEL1(LLEE, 4)        !! Neighbour-column split offset used for second connected cells.
      DOUBLE PRECISION, INTENT(IN) :: CA0           !! Plan area of the current element.
      DOUBLE PRECISION, INTENT(IN) :: CZ1(LLEE, 4)   !! Adjacent-cell node elevations by cell and face.
      DOUBLE PRECISION, INTENT(IN) :: CDELZ(ICBOT:ICTOP) !! Active-cell thicknesses.
      DOUBLE PRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP) !! Active-cell node elevations.
      DOUBLE PRECISION, INTENT(IN) :: CETA(ICBOT:ICTOP) !! Storage coefficient by active cell.
      DOUBLE PRECISION, INTENT(IN) :: DT            !! Timestep length.
      DOUBLE PRECISION, INTENT(IN) :: CDETA(ICBOT:ICTOP) !! Derivative of storage coefficient by active cell.
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Current pressure heads.
      DOUBLE PRECISION, INTENT(IN) :: CPSIN(ICBOT:ICTOP) !! Previous-timestep pressure heads.
      DOUBLE PRECISION, INTENT(IN) :: CF(ICBOT:ICTOP) !! Internal conductance contribution to the diagonal.
      DOUBLE PRECISION, INTENT(IN) :: CDF(ICBOT:ICTOP) !! Derivative of `CF` with respect to pressure head.
      DOUBLE PRECISION, INTENT(IN) :: CQ(ICBOT:ICTOP) !! Assembled cell source/sink terms.
      DOUBLE PRECISION, INTENT(IN) :: CBETM(ICBOT:ICTOP + 1) !! Vertical inter-cell conductance below each active cell.
      DOUBLE PRECISION, INTENT(IN) :: CDBETM(ICBOT:ICTOP + 1) !! Derivative of `CBETM` with respect to the lower cell.
      DOUBLE PRECISION, INTENT(IN) :: CDBTMM(ICBOT:ICTOP + 1) !! Derivative of `CBETM` with respect to the upper cell.
      DOUBLE PRECISION, INTENT(IN) :: CPSI1(LLEE, 4) !! Adjacent current pressure heads by cell and face.
      DOUBLE PRECISION, INTENT(IN) :: CPSIN1(LLEE, 4) !! Adjacent previous-timestep pressure heads by cell and face.
      DOUBLE PRECISION, INTENT(IN) :: CGAM1(LLEE, 4) !! Primary lateral coupling conductance.
      DOUBLE PRECISION, INTENT(IN) :: CDGAM1(LLEE, 4) !! Derivative of `CGAM1` with respect to local pressure head.
      DOUBLE PRECISION, INTENT(IN) :: CDGAM2(LLEE, 4) !! Derivative of `CGAM2` with respect to local pressure head.
      DOUBLE PRECISION, INTENT(IN) :: CGAM2(LLEE, 4) !! Secondary split-cell lateral coupling conductance.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CA(ICBOT:ICTOP) !! Lower diagonal for the tridiagonal column system.
      DOUBLE PRECISION, INTENT(OUT) :: CB(ICBOT:ICTOP) !! Diagonal for the tridiagonal column system.
      DOUBLE PRECISION, INTENT(OUT) :: CC(ICBOT:ICTOP) !! Upper diagonal for the tridiagonal column system.
      DOUBLE PRECISION, INTENT(OUT) :: CR(ICBOT:ICTOP) !! Right-hand side for the tridiagonal column system.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(OUT) :: H(ICBOT - 1:ICTOP + 1) !! Workspace for effective hydraulic heads.

      ! Locals
      DOUBLE PRECISION, PARAMETER :: SIGMA = 1.0D0, OMSIG = 1.0D0 - SIGMA
      INTEGER :: I, J, K, K1, P
      DOUBLE PRECISION :: CBETMI, CBETPI, CDBETP, CDBMMI, CDBTPP, CDFM, CDFP, CDG
      DOUBLE PRECISION :: CFI, CGI, DPSI, HI, HK, HK1, HM, HP, VODT

      !----------------------------------------------------------------------*

      ! Prepare effective hydraulic heads
      I = ICBOT - 1
      H(I) = zero

      DO I = ICBOT, ICTOP
         H(I) = SIGMA*CPSI(I) + OMSIG*CPSIN(I) + CZ(I)
      END DO

      I = ICTOP + 1
      H(I) = zero

      ! Set coefficients, omitting lateral terms
      DO I = ICBOT, ICTOP
         P = I + 1
         HM = H(I - 1)
         HI = H(I)
         HP = H(P)
         CFI = CF(I)
         CBETMI = CBETM(I)
         CBETPI = CBETM(P)
         CDBTPP = CDBETM(P)
         CDBMMI = CDBTMM(I)
         CDBETP = CDBTMM(P)
         CDFM = CDBMMI
         CDFP = CDBTPP
         VODT = CDELZ(I)*CA0/DT
         CGI = CETA(I)*VODT
         CDG = CDETA(I)*VODT
         DPSI = CPSI(I) - CPSIN(I)

         CA(I) = SIGMA*CBETMI - HI*CDFM + HM*CDBMMI
         CC(I) = SIGMA*CBETPI - HI*CDFP + HP*CDBTPP
         CB(I) = HM*CDBETM(I) - HI*CDF(I) + HP*CDBETP - &
                 (SIGMA*CFI + DPSI*CDG + CGI)
         CR(I) = -(HM*CBETMI - HI*CFI + HP*CBETPI - DPSI*CGI + CQ(I))
      END DO

      ! Add lateral terms
      face_loop: DO J = 1, 4

         IF (JELDUM(J) < 1 .OR. JCBC(J) == 9) CYCLE face_loop

         internal_cell_loop: DO I = ICBOT, ICTOP
            K = JCACN(J, I)
            IF (K == 0) CYCLE internal_cell_loop

            K1 = JCDEL1(K, J) + K
            HK = SIGMA*CPSI1(K, J) + OMSIG*CPSIN1(K, J) + CZ1(K, J)
            HK1 = SIGMA*CPSI1(K1, J) + OMSIG*CPSIN1(K1, J) + CZ1(K1, J)

            CB(I) = CB(I) + HK*CDGAM1(I, J) + HK1*CDGAM2(I, J)
            CR(I) = CR(I) - HK*CGAM1(I, J) - HK1*CGAM2(I, J)

         END DO internal_cell_loop

      END DO face_loop

   END SUBROUTINE VSINTC

!> Adds lower boundary-condition terms to the bottom VSS cell.
!>
!> `VSLOWR` applies the manual bottom boundary categories (`VS17`/`VS18`) to the
!> bottom cell of the column matrix assembled by [[vscolm]]. The required entry
!> condition is `CDELZ > 0`.
!>
!> Implemented behaviour is:
!>
!> | `JCBC` | Manual boundary type | Code behaviour |
!> |:-------|:---------------------|:---------------|
!> | 6 | prescribed column-base flow | Uses `CBF` directly. |
!> | 7 | prescribed column-base head | Applies a conductance term to head `CBH`. |
!> | 8 | free drainage | Currently falls through to zero lower-boundary flux. |
!> | other | no-flow/default | Zero lower-boundary flux. |
!>
!> For prescribed flow,
!>
!> \[
!>   q_b = CBF,\qquad {dq_b\over d\psi}=0.
!> \]
!>
!> For prescribed head, with bottom cell centre elevation `CZ`, pressure head
!> `CPSI`, saturated vertical conductivity `CKZS`, relative conductivity `CKR`,
!> and derivative `CDKR`,
!>
!> \[
!>   \Delta h = CBH - CZ - CPSI,\qquad
!>   K_{\Delta z} = {CKZS\over 0.5\,CDELZ},
!> \]
!>
!> \[
!>   q_b = K_{\Delta z}CKR\,\Delta h,\qquad
!>   {dq_b\over d\psi} = K_{\Delta z}(CDKR\,\Delta h - CKR).
!> \]
!>
!> The diagnostic/output lower flux is `CQV = q_b`. The linearised contribution
!> is inserted into the bottom-cell equation as
!>
!> \[
!>   CB \leftarrow CB + CA0\,{dq_b\over d\psi},\qquad
!>   CR \leftarrow CR - CA0\,q_b .
!> \]
!>
!> @note
!> None of this routine's dummy arguments carry an `INTENT` attribute in the
!> current declarations, unlike most other routines in this module.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written. |
!> | 1997-01-20 | RAH | 4.1 | Removed leading comments and lower-case code; combined `IF`-blocks; introduced the local `CQVDUM`. |
!> | 1997-01-31 | RAH | 4.1 | Passed data through arguments instead of `INCLUDE` blocks; declared `CDQDUM` as `DBLE` rather than `DOUBLEPRECISION`. |
!> @endhistory
   SUBROUTINE VSLOWR(JCBC, CA0, CZ, CDELZ, CKZS, CBF, CBH, CPSI, &
                     CKR, CDKR, CB, CR, CQV)
!
! Input arguments
      INTEGER :: JCBC           !! Bottom boundary type: 6 flow, 7 head, otherwise no-flow/free-drainage fallback.
      DOUBLEPRECISION :: CA0    !! Plan area of the current element.
      DOUBLEPRECISION :: CZ     !! Bottom-cell node elevation.
      DOUBLEPRECISION :: CDELZ  !! Bottom-cell thickness.
      DOUBLEPRECISION :: CKZS   !! Saturated vertical hydraulic conductivity for the bottom-cell soil.
      DOUBLEPRECISION :: CBF    !! Prescribed bottom-flow boundary value.
      DOUBLEPRECISION :: CBH    !! Prescribed bottom-head boundary value.
      DOUBLEPRECISION :: CPSI   !! Bottom-cell pressure head.
      DOUBLEPRECISION :: CKR    !! Bottom-cell relative hydraulic conductivity.
      DOUBLEPRECISION :: CDKR   !! Derivative of `CKR` with respect to pressure head.
!
! In+out arguments
      DOUBLEPRECISION :: CB  !! Bottom-cell matrix diagonal term.
      DOUBLEPRECISION :: CR  !! Bottom-cell right-hand side term.
!
! Output arguments
      DOUBLEPRECISION :: CQV   !! Bottom vertical boundary flux.
!
! Locals, etc
      DOUBLEPRECISION CDQDUM, CQVDUM, DH, KSODZ
!
!----------------------------------------------------------------------*
!
! column base flow (type 6)
      IF (JCBC .EQ. 6) THEN
         CQVDUM = CBF

         CDQDUM = zero
! column base head (type 7)
      ELSEIF (JCBC .EQ. 7) THEN
         DH = CBH - CZ - CPSI
         KSODZ = CKZS/(half*CDELZ)
         CQVDUM = KSODZ*CKR*DH

         CDQDUM = KSODZ*(CDKR*DH - CKR)
! no flow (970131: Check column base free drainage (type 8)!)
      ELSE
         CQVDUM = zero

         CDQDUM = zero

      END IF
      CQV = CQVDUM
      CB = CB + CA0*CDQDUM

      CR = CR - CA0*CQVDUM
   END SUBROUTINE VSLOWR

!> Adds stream-aquifer interaction terms to the column system.
!>
!> `VSSAI` applies the channel-aquifer exchange correction (added 1998-11 by
!> SPA) for boundary types `JCBC = 9` (no explicit banks) and `JCBC = 10`
!> (explicit banks) on one face of the column assembled by [[vscolm]]. Entry
!> conditions are `1 <= FACE <= 4`, `ICBOT <= ICBED+1, ICTOP`, and `CDELL > 0`.
!>
!> The lowest affected cell is `ICBOT` for `JCBC = 9` (the stream bed is
!> effectively at the base of the land element) or `ICBED+1` for `JCBC = 10`
!> (interaction starts above the explicit river-bed cell). For each affected
!> cell,
!>
!> \[
!>   \Delta h = CZS - CZ_c - CPSI_c .
!> \]
!>
!> The channel-to-aquifer contact area is limited when the channel water depth
!> is low or the cell would otherwise be losing water to a shallow channel:
!>
!> \[
!>   f =
!>   \begin{cases}
!>     \min(1,\;depadj/CDELZ_c), & \Delta h > 0,\\
!>     1, & \text{otherwise},
!>   \end{cases}
!>   \qquad A/L = {f\,CAIJ(FACE,c)\over CDELL}.
!> \]
!>
!> The exchange flux and its derivative are
!>
!> \[
!>   Q_c = CKIJ_c\,\Delta h\,(A/L),\qquad
!>   {dQ_c\over d\psi_c} = -CKIJ_c\,(A/L).
!> \]
!>
!> `CQH(FACE,c)` stores the diagnostic flux, and the linearised term is added
!> as `CB(c) += dQ_c/d\psi_c` and `CR(c) -= Q_c`.
!>
!> @note
!> `CDKIJ` is a dummy argument but not used in the current formula: the
!> derivative term omits the `CDKIJ*DH` contribution used in the original 1994
!> formula, matching the 1998-11 SPA revision noted in the header comments.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written; version 4.0 completed 1996-01-15. |
!> | 1997-01-21 | RAH | 4.1 | Declared `IDUM` as `INTEGER` rather than `DOUBLEPRECISION`; introduced `AOL`, `DH`, and `KIJ` to reduce the number of operations. |
!> | 1997-02-03 | RAH | 4.1 | Passed data through arguments instead of `INCLUDE` blocks; added explanatory comments. |
!> | 1997-02-11 | RAH | 4.1 | Removed the outputs `CQBKB` and `CQBKF` (now handled in [[vssim]]). |
!> | 1997-05-14 | RAH | 4.1 | Added the argument `FACE` and a leading dimension to `CAIJ` and `CQH`. |
!> | 1998-11-03 | SPA | - | Added the `depadj` channel-depth contact-area limit and changed the derivative definition to the current form. |
!> | 2026-04-06/07 | SvB | 4.6 | Added the `PURE` attribute; no other change. |
!> @endhistory
   PURE SUBROUTINE VSSAI(FACE, JCBC, ICBOT, ICTOP, ICBED, CDELL, CZ, &
                         CAIJ, CZS, CPSI, CKIJ, CDKIJ, CB, CR, CQH, depadj, cdelz)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: FACE                      !! Boundary face number, in `1:4`.
      INTEGER, INTENT(IN) :: JCBC                      !! Stream-aquifer boundary type, normally 9 or 10.
      INTEGER, INTENT(IN) :: ICBOT                     !! Bottom active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICTOP                     !! Top active VSS cell in the column.
      INTEGER, INTENT(IN) :: ICBED                     !! River-bed cell index used for bank interaction.
      DOUBLE PRECISION, INTENT(IN) :: CDELL             !! Distance scale normal to the stream-aquifer face.
      DOUBLE PRECISION, INTENT(IN) :: CZS               !! Adjacent channel water-surface elevation.
      DOUBLE PRECISION, INTENT(IN) :: depadj            !! Channel-depth adjustment for contact-area limiting.
      DOUBLE PRECISION, INTENT(IN) :: CZ(ICBOT:ICTOP)   !! Active-cell node elevations.
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICBOT:ICTOP) !! Current pressure heads.
      DOUBLE PRECISION, INTENT(IN) :: CAIJ(4, ICBOT:ICTOP) !! Face areas by face and active cell.
      DOUBLE PRECISION, INTENT(IN) :: cdelz(ICBOT:ICTOP) !! Active-cell thicknesses used in the contact-area limit.
      DOUBLE PRECISION, INTENT(IN) :: CKIJ(ICBOT:ICTOP) !! Lateral hydraulic conductivity terms on this face.
      DOUBLE PRECISION, INTENT(IN) :: CDKIJ(ICBOT:ICTOP) !! Unused conductivity derivatives retained for the legacy interface.

      ! In+out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: CB(ICBOT:ICTOP) !! Matrix diagonal terms updated by stream-aquifer exchange.
      DOUBLE PRECISION, INTENT(INOUT) :: CR(ICBOT:ICTOP) !! Right-hand side terms updated by stream-aquifer exchange.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT) :: CQH(4, ICBOT:ICTOP) !! Diagnostic lateral fluxes on the stream-aquifer face.

      ! Locals
      INTEGER :: ICL, IDUM
      DOUBLE PRECISION :: QDUM, DQDUM, AOL, DH, KIJ, DDUM

      !----------------------------------------------------------------------*

      ! set lowest cell in exposed bank face
      IF (JCBC == 9) THEN
         ! in effect stream bed is at base of current land element
         IDUM = ICBOT
      ELSE
         ! stream-aquifer interaction with banks
         IDUM = ICBED + 1
      END IF

      ! loop over appropriate cells
      cell_loop: DO ICL = IDUM, ICTOP

         DH = CZS - CZ(ICL) - CPSI(ICL)

         ! !!!!! change to calculation of AOL for flow out of channel
         ! limits flows if depth of water in channel is low, or zero
         ! SPA, 03/11/98
         DDUM = 1.0D0
         IF (GTZERO(DH)) DDUM = MIN(ONE, depadj/cdelz(ICL))

         AOL = (DDUM*CAIJ(FACE, ICL))/CDELL
         KIJ = CKIJ(ICL)

         ! !!!! SPA, 03/11/98.  Change definition of flow derivative
         ! DQDUM =   ( CDKIJ(ICL)*DH - KIJ ) * AOL
         DQDUM = -KIJ*AOL

         QDUM = KIJ*DH*AOL
         CQH(FACE, ICL) = QDUM

         CB(ICL) = CB(ICL) + DQDUM
         CR(ICL) = CR(ICL) - QDUM

      END DO cell_loop

   END SUBROUTINE VSSAI

!> Adds spring discharge terms to one VSS cell.
!>
!> `VSSPR` implements the spring boundary type (`JCBC(5) = 2`) for the single
!> cell selected by [[vsin]] from the manual `VS13b` spring source depth
!> `VSSPD`. The discharge elevation and spring coefficient are the `VS13b`
!> inputs passed here as `CZSP` and `CCS`.
!>
!> The spring is inactive while the hydraulic head in the source cell is below
!> the discharge elevation:
!> \[
!>   H - z_{\rm sp} = z_i + \psi_i - z_{\rm sp} < 0 .
!> \]
!> If the head is high enough, the routine computes the spring outflow as
!> \[
!>   Q_{\rm sp} = C_{\rm sp}\,K_r\,\left(z_i + \psi_i - z_{\rm sp}\right),
!> \]
!> where `CZ` is \(z_i\), `CPSI` is \(\psi_i\), `CKR` is the current relative
!> hydraulic conductivity from [[vsfunc]], and `CCS` is the spring coefficient.
!>
!> For an active spring, `CQSP` receives \(Q_{\rm sp}\), `CR` is increased by the
!> same flux, and `CB` is updated with the implemented linearisation term
!> `-CCS * CDKR`. For an inactive spring, `CQSP` is set to zero and the column
!> coefficients are unchanged.
!>
!> @note
!> This is not the full derivative of
!> \(C_{\rm sp}K_r(z_i+\psi_i-z_{\rm sp})\) with respect to pressure head,
!> which would include both the direct `CKR` term and the head-excess multiplier
!> on `CDKR`. The active implementation uses only `-CCS * CDKR`. Because the
!> activation test is `GEZERO`, a zero head excess gives zero spring flux but
!> still applies this coefficient term.
!> @endnote
!>
!> @note
!> None of this routine's dummy arguments carry an `INTENT` attribute in the
!> current declarations.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written. |
!> | 1997-01-20 | RAH | 4.1 | Removed the leading comments; introduced the local `DHDUM`. |
!> | 1997-01-27 | RAH | 4.1 | Passed data through arguments instead of `INCLUDE` blocks. |
!> @endhistory
   SUBROUTINE VSSPR(CZ, CZSP, CCS, CPSI, CKR, CDKR, CB, CR, CQSP)
!
! Input arguments
      DOUBLEPRECISION CZ    !! Spring-cell node elevation.
      DOUBLEPRECISION CZSP  !! Spring discharge elevation.
      DOUBLEPRECISION CCS   !! Spring conductance coefficient.
      DOUBLEPRECISION CPSI  !! Spring-cell pressure head.
      DOUBLEPRECISION CKR   !! Spring-cell relative hydraulic conductivity.
      DOUBLEPRECISION CDKR  !! Derivative of `CKR` with respect to pressure head.
!
! In+out arguments
      DOUBLEPRECISION CB !! Spring-cell matrix diagonal term.
      DOUBLEPRECISION CR !! Spring-cell right-hand side term.
!
! Output arguments
      DOUBLEPRECISION CQSP !! Spring discharge; zero when the spring is inactive.
!
! Locals, etc
      DOUBLEPRECISION DHDUM
!
!----------------------------------------------------------------------*
!
      DHDUM = CPSI + CZ - CZSP

      IF (GEZERO(DHDUM)) THEN

         CQSP = CCS*CKR*DHDUM
         CR = CR + CQSP

         CB = CB - CCS*CDKR

      ELSE

         CQSP = zero

      END IF
   END SUBROUTINE VSSPR

!> Adds the upper infiltration/exfiltration boundary to the top VSS cell.
!>
!> `VSUPPR` forms the top-boundary contribution for one VSS column. The input
!> `CDNET` is the net surface-water depth available over the timestep after
!> evaporation has been applied by [[vssim]], and `CKZS` is the vertical
!> saturated conductivity of the top cell. The routine uses the model flux
!> convention that `CQINF > 0` is upward from the subsurface to the surface, so
!> infiltration is negative. Entry conditions: `CDELZ > 0` and `DT > 0`.
!>
!> The water-availability limit is
!> \[
!>   q_{\rm in} = {d_{\rm net} \over \Delta t},
!> \]
!> the rate that would exhaust the available surface depth during the timestep
!> (Fortran name `QIN`). The hydraulic-capacity expression is
!> \[
!>   q_{\rm out} =
!>   {K_{zs} \over \Delta z/2}
!>   \left[\psi -
!>   \left(\max(d_{\rm net},0)+{\Delta z\over2}\right)\right],
!> \]
!> where `CPSI` is top-cell pressure head and `CDELZ` is top-cell thickness
!> (Fortran name `QOUT`).
!>
!> If available water is limiting (`q_in < -q_out`), the returned flux is
!> `CQINF = -q_in` and the derivative contribution is set to zero. Otherwise
!> the boundary is hydraulic-capacity limited, or exfiltrating, and
!> `CQINF = q_out` with derivative `CKZS/(CDELZ/2)`.
!>
!> The column-system updates are
!> \[
!>   CB \leftarrow CB - {K_{zs}\over\Delta z/2}\,A,\qquad
!>   CR \leftarrow CR + q_{\rm inf} A,
!> \]
!> except in the water-limited case where the coefficient term is zero.
!>
!> @note
!> `CKZS` is the saturated vertical conductivity passed from `VSK3D(SOIL,3)`;
!> the upper-boundary capacity does not use the current relative conductivity
!> `CKR` from [[vsfunc]]. Positive `CDNET` is treated as ponded depth in the
!> hydraulic head term. Negative `CDNET` can limit upward extraction through
!> `q_in = CDNET/DT`, but `MAX(CDNET,0)` means it does not impose a negative
!> surface-water head in `q_out`.
!> @endnote
!>
!> @note
!> None of this routine's dummy arguments carry an `INTENT` attribute in the
!> current declarations.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written; version 4.0 completed 1995-12-20. |
!> | 1997-01-20 | RAH | 4.1 | Removed leading/long comments and lower-case code; used the generic `MAX`; rearranged expressions; stopped including `AL.G`. |
!> | 1997-01-27 | RAH | 4.1 | Passed data through arguments instead of `COMMON`. |
!> | 1997-05-14 | RAH | 4.1 | Replaced `CDW + (CQP - CEW)*DT` with the single input `CDNET` (see [[vssim]]). |
!> | 1998-11-04 | RAH | 4.2 | Renamed the `DUM?` locals to `QIN` and similar. |
!> @endhistory
   SUBROUTINE VSUPPR(CA0, CDELZ, CKZS, DT, CDNET, CPSI, CB, CR, &
                     CQINF)
! Input arguments
      DOUBLEPRECISION CA0    !! Plan area of the current element.
      DOUBLEPRECISION CDELZ  !! Top-cell thickness.
      DOUBLEPRECISION CKZS   !! Saturated vertical hydraulic conductivity for the top-cell soil.
      DOUBLEPRECISION DT     !! Timestep length.
      DOUBLEPRECISION CDNET  !! Net available surface-water depth after evaporation.
      DOUBLEPRECISION CPSI   !! Top-cell pressure head.
! In+out arguments

      DOUBLEPRECISION CB !! Top-cell matrix diagonal term.
      DOUBLEPRECISION CR !! Top-cell right-hand side term.
! Output arguments

      DOUBLEPRECISION CQINF !! Calculated upward-positive infiltration/exfiltration rate.
! Locals, etc
!INTRINSIC MAX

      DOUBLEPRECISION QIN, QOUT, CDQINF, DZO2

      DZO2 = half*CDELZ
      QIN = CDNET/DT
      CDQINF = CKZS/DZO2

      QOUT = CDQINF*(CPSI - (MAX(CDNET, ZERO) + DZO2))
! infiltration (limited by available water) or evaporation

      IF (QIN .LT. -QOUT) THEN
         CQINF = -QIN

         CDQINF = ZERO
! infiltration (limited by soil properties) or exfiltration

      ELSE

         CQINF = QOUT

      END IF
! add into right-hand-side of column tridiagonal system
      CB = CB - CDQINF*CA0

      CR = CR + CQINF*CA0
   END SUBROUTINE VSUPPR

!> Distributes a prescribed well abstraction over screened VSS cells.
!>
!> `VSWELL` implements the well boundary type (`JCBC(5) = 1`) for the screen
!> interval `ICWLBT:ICWLTP`, which is derived in [[vsin]] from the manual
!> `VS12b` well-screen depths. The prescribed input `CQWIN` is the total well
!> abstraction rate in m3/s, read for the current timestep by [[vsprep]] from
!> the well data file.
!>
!> Each screened cell is first assigned a saturated lateral
!> conductivity-depth weight,
!> \[
!>   w_i = {K_{x,i}+K_{y,i} \over 2}\,\Delta z_i,\qquad
!>   W = \sum_{i=I_b}^{I_t} w_i .
!> \]
!> The available saturated thickness factor is then limited using the current
!> pressure head:
!> \[
!>   f_i =
!>   {\min\left(d_i,\max(\psi_i,0)\right) \over d_i},\qquad
!>   d_i = { \Delta z_i+\Delta z_{i+1} \over 2}.
!> \]
!>
!> The cell abstraction is
!> \[
!>   Q_i = Q_{\rm well}\,{w_i \over W}\,f_i ,
!> \]
!> so the total realised abstraction can be less than the prescribed value when
!> screened cells are partly or fully unsaturated. `CQWI(i)` stores the
!> corresponding areal rate \(Q_i/A\) in m/s, and `CR(i)` is increased by
!> \(Q_i\) for the column right-hand side.
!>
!> Entry conditions: `ICWLBT <= ICWLTP`;
!> `1 <= ICSOIL(ICWLBT:ICWLTP) <= NSEE`; and positive `CA0`, screened-cell
!> thicknesses including `CDELZ(ICWLTP+1)`, and a positive total
!> conductivity-depth weight \(W\) from
!> `VSK3D(ICSOIL(ICWLBT:ICWLTP),1:2)`.
!>
!> @note
!> The pressure-head reduction factor is evaluated explicitly. The routine does
!> not add a diagonal coefficient for the dependence of \(f_i\) on `CPSI(i)`, so
!> well abstraction changes affect the nonlinear iteration only through the next
!> column assembly. The sign convention assumes positive `CQWIN` is abstraction
!> from the VSS column.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-22 | GP | 4.0 | Written; version 4.0 completed 1995-02-28. |
!> | 1997-01-20 | RAH | 4.1 | Used generic intrinsics; introduced the local `QDUM`. |
!> | 1997-01-27 | RAH | 4.1 | Passed data through arguments instead of `INCLUDE` blocks. |
!> | 1997-02-07 | RAH | 4.1 | Redefined `CQWI` to be divided by `CA0`; removed the output `CQW`. |
!> | 1997-05-14 | RAH | 4.1 | Replaced `LLEE`/`CKIJS` with the new arguments `NSEE`, `ICSOIL`, and `VSK3D`; rearranged the `QDUM` expression. |
!> | 2026-04-06/07 | SvB | 4.6 | Added the `PURE` attribute; no other change. |
!> @endhistory
   PURE SUBROUTINE VSWELL(NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL, CA0, &
                          CDELZ, CQWIN, CPSI, CR, CQWI, RKZDUM)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NSEE                    !! Declared soil-type dimension for conductivity arrays.
      INTEGER, INTENT(IN) :: ICWLBT                  !! Bottom screened well cell.
      INTEGER, INTENT(IN) :: ICWLTP                  !! Top screened well cell.
      INTEGER, INTENT(IN) :: ICSOIL(ICWLBT:ICWLTP)   !! Soil type by screened cell.
      DOUBLE PRECISION, INTENT(IN) :: CA0             !! Plan area of the current element.
      DOUBLE PRECISION, INTENT(IN) :: CQWIN           !! Prescribed total well abstraction rate.
      DOUBLE PRECISION, INTENT(IN) :: CDELZ(ICWLBT:ICWLTP + 1) !! Screened-cell thicknesses plus the cell above the screen top.
      DOUBLE PRECISION, INTENT(IN) :: VSK3D(NSEE, 2)   !! Saturated x/y hydraulic conductivity by soil type.
      DOUBLE PRECISION, INTENT(IN) :: CPSI(ICWLBT:ICWLTP) !! Current pressure heads in screened cells.

      ! In+out arguments
      DOUBLE PRECISION, INTENT(INOUT) :: CR(ICWLBT:ICWLTP) !! Right-hand side terms updated with realised abstraction.

      ! Output arguments
      DOUBLE PRECISION, INTENT(OUT)   :: CQWI(ICWLBT:ICWLTP) !! Realised well abstraction rate per cell area.

      ! Workspace arguments
      DOUBLE PRECISION, INTENT(INOUT) :: RKZDUM(ICWLBT:ICWLTP) !! Workspace for conductivity-depth weights.

      ! Locals
      INTEGER :: ICL, SOIL
      DOUBLE PRECISION :: RKZTOT, DZDUM, PDUM, QDUM, RKZ

      !----------------------------------------------------------------------*

      ! The value of CQWIN is the prescribed abstraction rate (m3/s).
      ! The actual abstraction rate CQWI (m/s) may be less than this if some
      ! of the aquifer around the well screen becomes unsaturated
      ! (ie if CPSI(ICL) < DZDUM below).

      ! Calculate product of mean lateral hydraulic conductivity & cell depth
      ! Kept as scalar DO loop to maximize performance on small cell slices
      RKZTOT = ZERO

      rkz_loop: DO ICL = ICWLBT, ICWLTP
         SOIL = ICSOIL(ICL)
         RKZ = HALF*(VSK3D(SOIL, 1) + VSK3D(SOIL, 2))*CDELZ(ICL)
         RKZDUM(ICL) = RKZ
         RKZTOT = RKZ + RKZTOT
      END DO rkz_loop

      ! Calculate flow into well for each cell, & add into matrix coefficients
      well_flow_loop: DO ICL = ICWLBT, ICWLTP
         DZDUM = HALF*(CDELZ(ICL) + CDELZ(ICL + 1))
         PDUM = MIN(DZDUM, MAX(CPSI(ICL), ZERO))

         QDUM = CQWIN*(RKZDUM(ICL)/RKZTOT)*(PDUM/DZDUM)
         CQWI(ICL) = QDUM/CA0

         CR(ICL) = QDUM + CR(ICL)
      END DO well_flow_loop

   END SUBROUTINE VSWELL

END MODULE vs_sources

