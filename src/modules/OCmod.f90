!> @brief Overland and channel flow routing.
!>
!> `OCmod` implements the SHETRAN overland/channel flow (OC) component. It
!> reads OC input, builds channel link geometry and boundary-condition tables,
!> computes channel cross-section lookup tables, sets up row-wise indexing for
!> the implicit solver, and advances free-surface elevations and inter-element
!> flows during the simulation.
!>
!> The timestep solve is a row-swept implicit finite-difference system. Each
!> active row is assembled as a block tridiagonal coupling to the previous,
!> current, and next rows, inverted row by row, and then back-substituted in a
!> downward sweep. Channel conveyance is derived from the OC input-file
!> width/depth cross-section tables and Strickler roughness coefficients
!> through [[ocmod2:conveyan]]. Boundary categories, channel geometry, and
!> roughness controls correspond to the manual's Overland/Channel Module
!> section (records `OC1`-`OC41`).
!>
!> `STRXX` and `STRYY` (held in [[ocqdqmod]]) normally store the directional
!> Strickler roughness read from the OC records. A negative `STRXX` value is
!> allowed by the current checker as a surface-storage marker;
!> [[ocqdqmod:ocqdq]] interprets its magnitude as a millimetre-scale threshold
!> and substitutes fixed effective roughness values during face-flow
!> calculation.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed the implicit OC scheme, banks, hot-start state migration, boundary-condition arrays, row indexing, and merged channel cross-section lookup table `XSTAB`. |
!> | 2008-12 | JE | 4.3.5F90 | Created as part of the Fortran 90 conversion, replacing part of the legacy OC `.F` files. |
!> | 2026-05-10 | SvB | 4.6.1 | Moved the OC solver, water-surface, discharge, and index work arrays to allocatable storage (see [[initialise_ocsim_workspace]]). |
!> @endhistory
MODULE OCmod
   USE SGLOBAL
   USE AL_C, ONLY: IDUM, NBFACE, CWIDTH, ZBFULL, &
                   DUMMY, ZBEFF, ICMBK, BEXBK, QBKB, QBKF, ICMRF2, &
                   TIH, DHF, CLENTH, CLENTH, PNETTO, QH, QOC, LINKNS, ARXL
   USE AL_D, ONLY: DQ0ST, DQIST, DQIST2, OCNOW, OCNEXT, OCD, ESWA, QMAX, NOCBCC, &
                   NOCBCD, LCODEX, LCODEY, NOCTAB, OHB, OFB
   USE AL_G, ONLY: NGDBGN, NX, NY, ICMREF, ICMXY
   USE UTILSMOD, ONLY: HINPUT, FINPUT, AREADR, AREADI, JEMATMUL_VM, JEMATMUL_MM, INVERTMAT
   USE mod_load_filedata, ONLY: ALCHK, ALCHKI
   USE OCmod2, ONLY: GETHRF, GETQSA, GETQSA_ALL, SETHRF, SETQSA, CONVEYAN, OCFIX, XSTAB, &
                     HRFZZ, qsazz, INITIALISE_OCMOD  !these needed only for ad
   USE OCQDQMOD, ONLY: OCQDQ, STRXX, STRYY, HOCNOW, QOCF, XAFULL, COCBCD !, &  !REST NNEDED ONLY FOR AD

   IMPLICIT NONE

   ! Row-solver indexing (see [[ocind]])
   INTEGER            :: NELIND(NELEE)         !! Position of each element within its implicit-solver row.
   INTEGER            :: NROWF                  !! First non-empty OC solver row.
   INTEGER            :: NROWL                  !! Last non-empty OC solver row.
   INTEGER            :: NOCHB                  !! Number of OC head-boundary categories.
   INTEGER            :: NOCFB                  !! Number of OC flow-boundary categories.
   INTEGER            :: NROWEL(NELEE)         !! Contiguous list of OC elements in row-solver order.
   INTEGER            :: NROWST(NYEE + 1)        !! Row-start pointer into `NROWEL`.
   INTEGER            :: NXSECT(NLFEE)         !! Number of width-depth cross-section points for each channel link.

   ! Boundary-series and diagnostic-output timing state
   DOUBLEPRECISION    :: HOCLST                 !! Previous time-varying OC head-boundary time.
   DOUBLEPRECISION    :: HOCNXT                 !! Next time-varying OC head-boundary time.
   DOUBLEPRECISION    :: QFLAST                 !! Previous time-varying OC flow-boundary time.
   DOUBLEPRECISION    :: QFNEXT                 !! Next time-varying OC flow-boundary time.
   DOUBLEPRECISION    :: TDC                    !! First time for detailed OC diagnostic output; see the [[ocini]] shadowing warning.
   DOUBLEPRECISION    :: TFC                    !! Last time for detailed OC diagnostic output; see the [[ocini]] shadowing warning.
   DOUBLEPRECISION    :: HOCPRV(NOCTAB)        !! Previous head-boundary values by category.
   DOUBLEPRECISION    :: QOCFIN(NOCTAB)        !! Previous flow-boundary values by category.
   DOUBLEPRECISION    :: HOCNXV(NOCTAB)        !! Next head-boundary values by category.

   ! Channel cross-section tables (see [[ocxs]])
   DOUBLEPRECISION    :: XINH(NLFEE, NOCTAB)   !! Channel cross-section depths above bed.
   DOUBLEPRECISION    :: XINW(NLFEE, NOCTAB)   !! Channel cross-section widths.
   DOUBLEPRECISION    :: XAREA(NLFEE, NOCTAB)  !! Integrated channel cross-section areas.
   DOUBLEPRECISION    :: dtoc                   !! OC timestep in seconds.

   ! Persistent [[ocsim]] row-solver workspace, allocated once by [[initialise_ocsim_workspace]]
   INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: ijedum  !! Reshaped `ICMREF` neighbour-index slice passed to [[ocmod2:ocfix]].
   INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: ijedum2 !! Reshaped `ICMRF2` neighbour-index slice passed to [[ocmod2:ocfix]].
   DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: AA       !! Next-row block coefficients of the row-wise implicit matrix.
   DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: DD       !! Back-substituted water-level correction, by row position and row number.
   DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: BB       !! Current-row block coefficients of the row-wise implicit matrix.
   DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: GG       !! Forward-elimination constant term, by row position and row number.
   DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: CC       !! Previous-row block coefficients of the row-wise implicit matrix.
   DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: TM1      !! Scratch matrix product used while assembling a row's system.
   DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: TM2      !! Row system matrix, inverted in place by [[utilsmod:invertmat]].
   DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: inqsa    !! Current face-flow buffer passed to [[ocmod2:ocfix]].
   DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: GGGETQSA !! Corrected face-flow buffer returned by [[ocmod2:ocfix]].
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: FF       !! Right-hand-side vector for the current row.
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TV1      !! Scratch vector product used while assembling a row's right-hand side.
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TV2      !! Row right-hand-side vector, inverted in place alongside `TM2`.
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: inhrf    !! Current water-level buffer passed to [[ocmod2:ocfix]].
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: GGGETHRF !! Corrected water-level buffer returned by [[ocmod2:ocfix]].
   DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE :: EE       !! Forward-elimination coefficient relating a row's correction to the next row's.

   PRIVATE

   PUBLIC :: OCINI, OCSIM, OCLTL, LINKNO, & !REST ARE PUBLIC FOR AD ONLY
             qfnext, hoclst, hocprv, qocfin, hocnxt, hocnxv

CONTAINS

!> @brief Controls OC component initialisation.
!>
!> `OCINI` checks static dimensions and topology, reads the OC input file,
!> validates roughness and cross-section data, opens boundary files,
!> initialises OC state held in [[OCmod2]] and this module's row-solver
!> workspace, builds channel cross-section tables through [[OCXS]], and
!> prepares row indices through [[OCIND]].
!>
!> Entry requirements are the OC/frame array limits being positive
!> (`NELEE`, `NLFEE`, `NXEE`, `NY`, `NOCTAB`), at least two internal
!> cross-section table rows (`NXSCEE >= 2`), and an element index range with
!> `total_no_elements >= NGDBGN`.
!>
!> The routine uses frame geometry and topology from `ICMREF`, `ICMBK`,
!> `ICMXY`, `LCODEX`, `LCODEY`, `NBFACE`, and `LINKNS`, plus OC input/output
!> units `OCD`, `OHB`, `OFB`, and `PRI`. It treats `ZGRUND` as input for
!> land/bank elements and fills the link entries from the OC/cross-section
!> setup.
!>
!> Initialised shared outputs include boundary-condition counts and codes
!> (`NOCHB`, `NOCFB`, `NOCBCC`, `NOCBCD`, `COCBCD`), hydraulic geometry
!> (`HRF`, `CWIDTH`, `ZBEFF`, `ZBFULL`, `NXSECT`, `XINH`, `XINW`, `XAREA`,
!> `XAFULL`, `XSTAB`), Strickler/roughness fields (`STRXX`, `STRYY`), timing controls
!> (`TDC`, `TFC`), and row-index arrays (`NELIND`, `NROWEL`, `NROWST`,
!> `NROWF`, `NROWL`) used by the OC implicit row solver.
!>
!> @warning
!> The local `TDC` and `TFC` declared here shadow the module-level variables
!> of the same name that [[OCSIM]] reads to gate diagnostic printing.
!> `OCREAD` fills only these local copies, so the module's `TDC`/`TFC` are
!> never explicitly assigned by the current initialisation path. This
!> documents current behaviour; it was not repaired in this transfer.
!> @endwarning
   SUBROUTINE OCINI()

      IMPLICIT NONE

      ! Locals
      INTEGER :: KONT                                !! Print/output control read by [[ocread]]; odd values enable verbose echoing.
      DOUBLE PRECISION :: DDUM1(NOCTAB), DDUM2(NOCTAB, NOCTAB) !! Discarded roughness/cross-section scratch passed to [[ocread]].
      DOUBLE PRECISION :: TDC, TFC                    !! Shadow the module-level `TDC`/`TFC`; see the routine's warning.
      LOGICAL :: LDUM1(NELEE)                        !! Discarded per-element check-result scratch passed to [[occhk1]].

      !----------------------------------------------------------------------*

      CALL OCCHK0()

      ! Call to check constraints using AD-aliasing safe interface
      CALL OCCHK1(SIZE(LDUM1), LDUM1)

      ! Input data & associated requirements
      CALL OCREAD(KONT, TDC, TFC, DDUM1, DDUM2)
      CALL OCCHK2(DUMMY, DDUM1, NELEE, LDUM1)

      ! Boundary data files
      ! Read title lines if applicable
      IF (NOCHB > 0) READ (OHB, *)
      IF (NOCFB > 0) READ (OFB, *)

      CALL INITIALISE_OCMOD()
      CALL INITIALISE_OCSIM_WORKSPACE()

      ! Cross-section tables & effective bed elevations
      IF (total_no_links > 0) THEN
         IF (MOD(KONT, 2) == 1) WRITE (PPPRI, 9100) NXSCEE
         CALL OCXS()
      END IF

      ! Indicies for Thomas algorithm
      CALL OCIND(BEXBK, NROWF, NROWL, NROWST, NELIND, NROWEL)

      RETURN

      ! FORMAT statements
9100  FORMAT(/5X, 'Size of internal tables for channel conveyance, etc', '  NXSCEE =', I6)

   END SUBROUTINE OCINI

!> @brief Allocates the persistent [[ocsim]] row-solver work arrays.
!>
!> These arrays were formerly automatic local arrays in [[ocsim]]. They are too
!> large for the stack on some compilers/runs, but allocating them on every
!> [[ocsim]] call is expensive because [[ocsim]] is called every timestep.
!> Keeping them as module work arrays preserves heap storage without repeated
!> allocation in the timestep loop.
!>
!> [[ocini]] calls this routine once, after `NX`, `NY`, `total_no_elements`,
!> `NELEE`, and `NLFEE` have been established. [[ocsim]] still clears every
!> array on each call before use.
!>
!> @warning
!> The `ALLOCATED` guard makes this a one-shot initialiser: there is no
!> resizing or deallocation path, so a later change in `NX`, `NY`,
!> `total_no_elements`, `NELEE`, or `NLFEE` within the same process would not
!> be reflected in these arrays.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-05-10 | SvB | 4.6.1 | Added this allocator while moving `AA`, `DD`, `FF`, `BB`, `GG`, `CC`, `EE`, `TM1`, `TM2`, `TV1`, `TV2`, `inhrf`, `GGGETHRF`, `inqsa`, `GGGETQSA`, `ijedum`, and `ijedum2` from automatic locals in [[ocsim]] to allocatable module state. |
!> @endhistory
   SUBROUTINE INITIALISE_OCSIM_WORKSPACE()
      IMPLICIT NONE

      IF (.NOT. ALLOCATED(ijedum)) THEN
         ! NELEE/NLFEE (not the active NX/NY-derived counts) size these two arrays, matching OCSIM's neighbour-index domain
         ALLOCATE (ijedum(nelee, 4, 2:3), ijedum2(nlfee, 3, 2))
         ALLOCATE (AA(NX*4, NX*4), DD(NX*4, NY))
         ALLOCATE (FF(NX*4))
         ALLOCATE (BB(NX*4, NX*4), GG(NX*4, NY))
         ALLOCATE (CC(NX*4, NX*4))
         ALLOCATE (EE(NX*4, NX*4, NY))
         ALLOCATE (TM1(NX*4, NX*4), TM2(NX*4, NX*4))
         ALLOCATE (TV1(NX*4), TV2(NX*4))
         ALLOCATE (inhrf(total_no_elements))
         ALLOCATE (GGGETHRF(total_no_elements))
         ALLOCATE (inqsa(total_no_elements, 4))
         ALLOCATE (GGGETQSA(total_no_elements, 4))
      END IF

   END SUBROUTINE INITIALISE_OCSIM_WORKSPACE

!> @brief Assembles one element row of the implicit OC matrix.
!>
!> Given the current element, boundary type, water level, storage area,
!> rainfall, evaporation, exchange flow, and previously calculated flow
!> derivatives, `OCABC` fills the lower, central, upper, and right-hand-side
!> coefficients used by the row-wise implicit solver in [[OCSIM]].
!>
!> The routine uses the element topology in `ICMREF`, multi-link node
!> expansion in `ICMRF2`, flow derivatives `DQ0ST`, `DQIST`, and `DQIST2`,
!> bank exchange flows `QBKB` and `QBKF`, current face flows from
!> [[ocmod2:getqsa]], row indices `NELIND`, and channel cross-section tables
!> `XINH`/`XINW`.
!>
!> For fixed-head boundary types `IBC=3` and `IBC=9`, the assembled row simply
!> enforces the prescribed head increment:
!>
!> \[
!> \Delta H = HNOW - H,\qquad BB_{IND}=1,\qquad FF=HNOW-H.
!> \]
!>
!> Otherwise the unknown is the water-level correction for the current
!> element and its neighbours. The current water depth is
!>
!> \[
!> H = Z - ZG.
!> \]
!>
!> For land/bank elements the storage area is `AREAE`. For channel links
!> below bankfull level, the storage width is linearly interpolated from the
!> cross-section table and multiplied by link length:
!>
!> \[
!> A_s =
!> CL\left(W_m +
!> \frac{H-H_m}{H_i-H_m}(W_i-W_m)\right),
!> \]
!>
!> where \(H_m \le H < H_i\), `W_m=XINW(link,m)`, and `W_i=XINW(link,i)`.
!> The storage term contributes
!>
!> \[
!> BB_{IND} \leftarrow -A_s/DTOC.
!> \]
!>
!> Rainfall, evaporation, bank exchange, and external exchange are assembled
!> on the right-hand side as
!>
!> \[
!> FF = -AREAE\,(P_{net}+Q_H-E_{sw}) + Q_{bank},
!> \]
!>
!> with `Q_H=QHE` only for non-link elements. For links, rainfall is
!> suppressed when `H < 1D-8`, and
!>
!> \[
!> Q_{bank}=QBKB_{1}+QBKF_{1}+QBKB_{2}+QBKF_{2}.
!> \]
!>
!> Each face flow is taken positive into the current element. For face
!> \(f\), the previously calculated linearisation is applied as
!>
!> \[
!> Q_f^{n+1} \approx Q_f + DQ0ST_f\,\Delta H_i
!>                  + DQIST_f\,\Delta H_j,
!> \]
!>
!> so the current-element coefficient and residual are updated by
!>
!> \[
!> BB_{IND} \leftarrow BB_{IND}+DQ0ST_f,\qquad
!> FF \leftarrow FF-Q_f.
!> \]
!>
!> A single adjacent element receives `DQIST` in the same row (`BB`), a later
!> row (`AA`), or an earlier row (`CC`) according to its row number. For a
!> multi-link junction, `ICMREF` contains a negative pointer to `ICMRF2`; the
!> same operation is applied to each connected link using `DQIST2`.
   SUBROUTINE OCABC(IND, IROW, IELZ, NSV, NCR, NPR, IBC, N, AREAE, &
                    ZG, CL, ZBF, Z, PNETT, QHE, ESWAE, HNOW, AA, BB, CC, FF)

      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)          :: IND    !! Row position of the current element.
      INTEGER, INTENT(IN)          :: IROW   !! Row number of the current element.
      INTEGER, INTENT(IN)          :: IELZ   !! Current element number.
      INTEGER, INTENT(IN)          :: NSV    !! Number of elements in the next (following) row.
      INTEGER, INTENT(IN)          :: NCR    !! Number of elements in the current row.
      INTEGER, INTENT(IN)          :: NPR    !! Number of elements in the previous row.
      INTEGER, INTENT(IN)          :: IBC    !! Boundary-condition type for the current element, or 0.
      INTEGER, INTENT(IN)          :: N      !! Number of cross-section table points for the current element's link.
      DOUBLE PRECISION, INTENT(IN) :: AREAE  !! Plan storage area of the current (non-link) element.
      DOUBLE PRECISION, INTENT(IN) :: ZG     !! Ground/bed elevation of the current element.
      DOUBLE PRECISION, INTENT(IN) :: CL     !! Channel-link length, used for link storage width.
      DOUBLE PRECISION, INTENT(IN) :: ZBF    !! Bankfull elevation of the current element's link.
      DOUBLE PRECISION, INTENT(IN) :: Z      !! Current water-surface elevation.
      DOUBLE PRECISION, INTENT(IN) :: PNETT  !! Net rainfall rate onto the current element.
      DOUBLE PRECISION, INTENT(IN) :: QHE    !! Exchange flow rate for the current (non-link) element.
      DOUBLE PRECISION, INTENT(IN) :: ESWAE  !! Evaporation rate from the current element's surface water.
      DOUBLE PRECISION, INTENT(IN) :: HNOW   !! Prescribed head value for a fixed-head boundary.
      DOUBLE PRECISION, INTENT(OUT):: AA(NXOCEE) !! Next-row coefficients for elements adjacent to `IELZ`.
      DOUBLE PRECISION, INTENT(OUT):: BB(NCR)    !! Current-row coefficients for elements adjacent to `IELZ`.
      DOUBLE PRECISION, INTENT(OUT):: CC(NXOCEE) !! Previous-row coefficients for elements adjacent to `IELZ`.
      DOUBLE PRECISION, INTENT(OUT):: FF         !! Right-hand-side residual for the current element's row equation.

      ! Local Variables
      INTEGER                      :: I, IBR, IFACE, IM, J, JEL, JFACE, JND, JROW
      DOUBLE PRECISION             :: AR, BKDUM, DQ0, DQI, H, HI, HM, PDUM, Q
      DOUBLE PRECISION             :: QHDUM, WI, WM
      LOGICAL                      :: BLINK, TEST

      !----------------------------------------------------------------------*

      ! ----- INITIALIZE OUTPUT ARRAYS & GET WATER DEPTH
      ! Performance Rollback: Explicit DO loops bypass dope-vector overhead for micro-arrays
      IF (NSV > 0) THEN
         DO I = 1, NSV
            AA(I) = ZERO
         END DO
      END IF

      DO I = 1, NCR
         BB(I) = ZERO
      END DO

      IF (NPR > 0) THEN
         DO I = 1, NPR
            CC(I) = ZERO
         END DO
      END IF

      H = Z - ZG

      ! ----- HEAD BOUNDARY
      IF (IBC == 3 .OR. IBC == 9) THEN
         BB(IND) = ONE
         FF = HNOW - H
         RETURN
      END IF

      ! ----- IS THE CURRENT ELEMENT A LINK?
      BLINK = (ICMREF(IELZ, 1) == 3)

      ! ----- PUT STORAGE TERM INTO CENTRAL COEFFICIENT FOR CURRENT ELEMENT
      TEST = BLINK
      IF (TEST) TEST = (Z < ZBF)

      IF (TEST) THEN
         ! * note requirements: XINH(IEL,1)=0; XINH(IEL,N).GE.ZBF-ZG
         search_loop: DO I = 2, N
            HI = XINH(IELZ, I)
            IF (H < HI) THEN
               IM = I - 1
               HM = XINH(IELZ, IM)
               WM = XINW(IELZ, IM)
               WI = XINW(IELZ, I)
               AR = CL*(WM + (WI - WM)*((H - HM)/(HI - HM)))
               EXIT search_loop
            END IF
         END DO search_loop
      ELSE
         AR = AREAE
      END IF

      BB(IND) = -AR/DTOC

      ! ----- PUT PRECIPITATION, EVAPORATION AND EXCHANGE FLOWS INTO RHS
      PDUM = PNETT
      IF (BLINK) THEN
         IF (H < 1.0D-8) PDUM = ZERO
         BKDUM = QBKB(IELZ, 1) + QBKF(IELZ, 1) + QBKB(IELZ, 2) + QBKF(IELZ, 2)
         QHDUM = ZERO
      ELSE
         BKDUM = ZERO
         QHDUM = QHE
      END IF

      FF = -AREAE*(PDUM + QHDUM - ESWAE) + BKDUM

      ! ----- LOOP OVER ADJACENT ELEMENTS
      face_loop: DO IFACE = 1, 4
         JEL = ICMREF(IELZ, IFACE + 4)
         JFACE = ICMREF(IELZ, IFACE + 8)

         ! --- GET FLOW AND DERIVATIVE (+VE INTO ELEMENT)
         Q = GETQSA(ielz, IFACE)
         DQ0 = DQ0ST(IELZ, IFACE)

         ! --- ADD INTO COEFFICIENTS FOR CURRENT ELEMENT
         BB(IND) = BB(IND) + DQ0
         FF = FF - Q

         ! --- TEST FOR SINGLE ADJACENT ELEMENT
         IF (JEL > 0) THEN
            JROW = ICMREF(JEL, 3)
            JND = NELIND(JEL)
            DQI = DQIST(IELZ, IFACE)

            !        ADD DERIVATIVE TO COEFFICIENT FOR ADJACENT ELEMENT
            IF (JROW == IROW) BB(JND) = BB(JND) + DQI
            IF (JROW > IROW) AA(JND) = AA(JND) + DQI
            IF (JROW < IROW) CC(JND) = CC(JND) + DQI

            ! --- SIMILARLY FOR MULTIPLE ADJACENT LINKS
         ELSE IF (JEL < 0) THEN
            IBR = -JEL
            DO J = 1, 3
               JEL = ICMRF2(IBR, J)
               IF (JEL > 0) THEN
                  JROW = ICMREF(JEL, 3)
                  JND = NELIND(JEL)
                  DQI = DQIST2(IBR, J)

                  IF (JROW == IROW) BB(JND) = BB(JND) + DQI
                  IF (JROW > IROW) AA(JND) = AA(JND) + DQI
                  IF (JROW < IROW) CC(JND) = CC(JND) + DQI
               END IF
            END DO
         END IF
      END DO face_loop

   END SUBROUTINE OCABC

!> @brief Reads and builds OC boundary-condition metadata.
!>
!> `JEOCBC` maps gridded head, flux, polynomial, channel-link, and
!> impermeable boundary-condition definitions onto `NOCBCD` and `NOCBCC`,
!> including extra bank elements where bank flow is represented.
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NELEE >= total_no_elements` | Element-indexed workspace is large enough. |
!> | `NXEE >= max(NX,1)` | Grid-code workspace is large enough. |
!> | `NY >= 1`, `NGDBGN >= 1`, `NOCTAB >= 1` | Active grid, land-element start, and boundary table capacity exist. |
!> | `ICMXY(1:NX,1:NY) <= total_no_elements` and `ICMREF(1:total_no_elements,5:8) <= total_no_elements` | Grid and neighbour indices are in range where positive. |
!> | `7 <= LCODEX(x,y) <= 11` | `LINKNO(x,y,.TRUE.)` must return a valid link index below `NGDBGN`. |
!> | `7 <= LCODEY(x,y) <= 11` | `LINKNO(x,y,.FALSE.)` must return a valid link index below `NGDBGN`. |
!> | `OCD`, `PRI` | Open formatted input and diagnostic output units. |
!>
!> On exit, `IXER` is only increased, every `NOCBCC(element)` is either zero
!> or a boundary-condition index, and a clean exit satisfies
!> `NOCBC <= NOCTAB`, `1 <= NOCBCD(1:NOCBC,1) <= total_no_elements`, and
!> `1 <= NOCBCD(1:NOCBC,3) <= 11`.
!>
!> `NOCBCD` is the boundary-condition table:
!>
!> | Column | Stored value |
!> |:-------|:-------------|
!> | 1 | Element or channel-link index carrying the boundary condition. |
!> | 2 | Boundary face number, where applicable. |
!> | 3 | Boundary-condition type code. |
!> | 4 | Category number within that type. |
!>
!> For gridded head, flux, and polynomial boundaries, a positive category
!> `ICAT` read for element `e` creates a new boundary row
!>
!> \[
!> b \leftarrow b+1,\qquad NOCBCC_e=b,\qquad
!> NOCBCD_{b,:}=(e,\ face,\ type,\ ICAT).
!> \]
!>
!> Head boundaries use type 3 and no face (`face=0`), while flux and
!> polynomial boundaries use the element's stored boundary face `NBFACE(e)`
!> with types 4 and 5. Polynomial boundary rows also receive the five
!> coefficients read from record `OC28`:
!>
!> \[
!> COCBCD_{1:5,b}=a_{1:5}(ICAT).
!> \]
!>
!> Channel-link boundary codes are taken directly from `LCODEX`/`LCODEY`
!> when their values are 7:11. The corresponding link is found with
!> `LINKNO`, a row is added with `NOCBCD(:,1)=link` and `NOCBCD(:,3)=type`,
!> and type 9 and 10 entries increment the head-boundary and flux-boundary
!> counts respectively. Link-specific parameters for types 7:11 are filled
!> later by [[OCPLF]].
!>
!> Internal impermeable grid boundaries use type 1. For each impermeable
!> west/south grid boundary, `JEOCBC` creates reciprocal rows for the two
!> adjacent elements and extends the impermeable condition across the ends of
!> any adjacent bank elements. The reciprocal face is taken from
!> `ICMREF(:,9:12)` so the table remains consistent with the topology built
!> by [[frmod:frind]].
!>
!> Boundary type codes are:
!>
!> | Type | Meaning |
!> |:-----|:--------|
!> | 1 | Internal impermeable grid boundary. |
!> | 3 | Time-varying grid head boundary. |
!> | 4 | Time-varying grid flux boundary. |
!> | 5 | Polynomial grid boundary. |
!> | 7 | Channel weir boundary. |
!> | 8 | Channel river/resistance plus weir boundary. |
!> | 9 | Time-varying channel head boundary. |
!> | 10 | Time-varying channel flow boundary. |
!> | 11 | Polynomial channel boundary. |
   SUBROUTINE JEOCBC(IXER, NOCBC)

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(INOUT)       :: IXER  !! OC input-error count; only ever increased here.
      INTEGER, INTENT(OUT)         :: NOCBC !! Total number of OC boundary-condition rows built.

      ! Local Variables
      INTEGER                      :: BANK, I, IBANK, IBC, IBC0, IBK, ICAT
      INTEGER                      :: IELY, IFACE, J, JBANK, JBC, JEL, K
      INTEGER                      :: KFACE, NOCPB, TYPEE
      DOUBLE PRECISION             :: ADUM(5)
      LOGICAL                      :: TEST
      CHARACTER(LEN=77)            :: MSG

      !----------------------------------------------------------------------*

      ! NUMBER OF CATEGORIES FOR EACH TYPE
      READ (OCD, *)
      READ (OCD, *) NOCHB, NOCFB, NOCPB

      ! INITIALIZATION
      NOCBC = 0

      ! Vectorized zeroing for large array
      NOCBCC(1:total_no_elements) = 0

      ! HEAD BOUNDARY (TYPE 3)
      IF (NOCHB > 0) THEN
         MSG = 'ERROR IN OC HEAD BOUNDARY GRID'
         CALL AREADI(IDUM, 0, OCD, PPPRI, NOCHB)

         DO IELY = NGDBGN, total_no_elements
            ICAT = IDUM(IELY)
            IF (ICAT < 0 .OR. ICAT > NOCHB) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1020, PPPRI, IELY, 0, MSG)
            ELSE IF (ICAT > 0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC > NOCTAB) CYCLE
               NOCBCC(IELY) = NOCBC
               NOCBCD(NOCBC, 1) = IELY
               NOCBCD(NOCBC, 2) = 0
               NOCBCD(NOCBC, 3) = 3
               NOCBCD(NOCBC, 4) = ICAT
            END IF
         END DO
      END IF

      ! FLUX BOUNDARY (TYPE 4)
      IF (NOCFB > 0) THEN
         MSG = 'ERROR IN OC FLUX BOUNDARY GRID'
         CALL AREADI(IDUM, 0, OCD, PPPRI, NOCFB)

         DO IELY = NGDBGN, total_no_elements
            ICAT = IDUM(IELY)
            IF (ICAT < 0 .OR. ICAT > NOCFB) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1021, PPPRI, IELY, 0, MSG)
            ELSE IF (ICAT > 0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC > NOCTAB) CYCLE
               NOCBCC(IELY) = NOCBC
               NOCBCD(NOCBC, 1) = IELY
               NOCBCD(NOCBC, 2) = NBFACE(IELY)
               NOCBCD(NOCBC, 3) = 4
               NOCBCD(NOCBC, 4) = ICAT
            END IF
         END DO
      END IF

      ! POLYNOMIAL FUNCTION BOUNDARY (TYPE 5)
      IF (NOCPB > 0) THEN
         IBC0 = NOCBC
         MSG = 'ERROR IN OC POLYNOMIAL FUNCTION BOUNDARY GRID'
         CALL AREADI(IDUM, 0, OCD, PPPRI, NOCPB)

         DO IELY = NGDBGN, total_no_elements
            ICAT = IDUM(IELY)
            IF (ICAT < 0 .OR. ICAT > NOCPB) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1022, PPPRI, IELY, 0, MSG)
            ELSE IF (ICAT > 0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC > NOCTAB) CYCLE
               NOCBCC(IELY) = NOCBC
               NOCBCD(NOCBC, 1) = IELY
               NOCBCD(NOCBC, 2) = NBFACE(IELY)
               NOCBCD(NOCBC, 3) = 5
               NOCBCD(NOCBC, 4) = ICAT
            END IF
         END DO

         MSG = 'Error reading polynomial function data in OC'
         READ (OCD, *)

         DO I = 1, NOCPB
            READ (OCD, *) ICAT, ADUM
            IF (ICAT /= I) THEN
               IXER = IXER + 1
               CALL ERROR(EEERR, 1031, PPPRI, IELY, 0, MSG)
            ELSE
               DO IBC = IBC0 + 1, MIN(NOCBC, NOCTAB)
                  TEST = (NOCBCD(IBC, 4) == I)
                  IF (TEST) COCBCD(1:5, IBC) = ADUM
               END DO
            END IF
         END DO
      END IF

      ! SET CHANNEL LINK BOUNDARY TYPES (other data will follow)
      x_link_loop: DO I = 1, NX
         y_link_loop: DO J = 1, NY
            DO K = 0, 1
               TYPEE = LCODEX(I, J)*(1 - K) + LCODEY(I, J)*K
               IF (TYPEE >= 7 .AND. TYPEE <= 11) THEN
                  IELY = LINKNO(I, J, K == 0)
                  NOCBC = NOCBC + 1
                  IF (NOCBC <= NOCTAB) THEN
                     NOCBCC(IELY) = NOCBC
                     NOCBCD(NOCBC, 1) = IELY
                     NOCBCD(NOCBC, 3) = TYPEE
                     IF (TYPEE == 9) NOCHB = NOCHB + 1
                     IF (TYPEE == 10) NOCFB = NOCFB + 1
                  END IF
               END IF
            END DO
         END DO y_link_loop
      END DO x_link_loop

      ! SET INTERNAL IMPERMEABLE GRID BOUNDARY CONDITIONS (TYPE 1)
      ! NB Impermeability extended across ends of any adjacent bank elements
      IBC0 = NOCBC
      x_grid_loop: DO I = 1, NX
         y_grid_loop: DO J = 1, NY
            DO IFACE = 3, 4
               TYPEE = LCODEX(I, J)*(4 - IFACE) + LCODEY(I, J)*(IFACE - 3)
               IF (TYPEE == 1) THEN
                  IELY = ICMXY(I, J)
                  JEL = 0
                  IF (IELY > 0) JEL = ICMREF(IELY, 4 + IFACE)

                  IF (JEL > 0) THEN
                     NOCBC = NOCBC + 1
                     IF (NOCBC <= NOCTAB) THEN
                        NOCBCC(IELY) = NOCBC
                        NOCBCD(NOCBC, 1) = IELY
                        NOCBCD(NOCBC, 2) = IFACE
                     END IF

                     NOCBC = NOCBC + 1
                     IF (NOCBC <= NOCTAB) THEN
                        NOCBCC(JEL) = NOCBC
                        NOCBCD(NOCBC, 1) = JEL
                        NOCBCD(NOCBC, 2) = ICMREF(IELY, 8 + IFACE)
                     END IF

                     DO BANK = 2, 1, -1
                        KFACE = 9 - IFACE - 2*BANK
                        IBANK = ICMREF(IELY, 4 + KFACE)
                        IBK = 0
                        IF (IBANK > 0) IBK = ICMREF(IBANK, 1)

                        IF (IBK == BANK) THEN
                           NOCBC = NOCBC + 1
                           IF (NOCBC <= NOCTAB) THEN
                              NOCBCC(IBANK) = NOCBC
                              NOCBCD(NOCBC, 1) = IBANK
                              NOCBCD(NOCBC, 2) = IFACE
                           END IF

                           NOCBC = NOCBC + 1
                           IF (NOCBC > NOCTAB) CYCLE

                           JBANK = ICMREF(IBANK, 4 + IFACE)
                           NOCBCC(JBANK) = NOCBC
                           NOCBCD(NOCBC, 1) = JBANK
                           NOCBCD(NOCBC, 2) = ICMREF(IBANK, 8 + IFACE)
                        END IF
                     END DO
                  END IF
               END IF
            END DO
         END DO y_grid_loop
      END DO x_grid_loop

      ! Vectorized setting types and categories
      IF (NOCBC > IBC0) THEN
         NOCBCD(IBC0 + 1:MIN(NOCBC, NOCTAB), 3) = 1
         NOCBCD(IBC0 + 1:MIN(NOCBC, NOCTAB), 4) = 1
      END IF

      ! CHECK
      IF (NOCBC > NOCTAB) THEN
         IXER = IXER + 1
         WRITE (MSG, "('Number of OC boundary conditions NOCBC =',I4,2X,'exceeds array size NOCTAB =',I4)") NOCBC, NOCTAB
         CALL ERROR(EEERR, 1050, PPPRI, 0, 0, MSG)
      END IF

      DO IBC = 1, MIN(NOCBC, NOCTAB)
         IELY = NOCBCD(IBC, 1)
         JBC = NOCBCC(IELY)
         IF (JBC /= IBC) THEN
            IXER = IXER + 1
            WRITE (MSG, "('Element has multiple OC boundary conditions (types',I2,' and',I2,')')") NOCBCD(IBC, 3), NOCBCD(JBC, 3)
            CALL ERROR(EEERR, 1059, PPPRI, IELY, 0, MSG)
         END IF
      END DO

   END SUBROUTINE JEOCBC

!> @brief Checks OC file units, array bounds, and global entity counts.
!>
!> This is the first OC validation pass and ensures the output/input units
!> are usable and that compiled dimensions are large enough for the current
!> grid, channel-link, cross-section, and boundary-condition counts.
!>
!> Checks performed:
!>
!> | Check | Requirement |
!> |:------|:------------|
!> | `PRI`, `OCD` | Open formatted diagnostic/input units. |
!> | `NELEE` | At least `max(NX,total_no_elements)`. |
!> | `NLFEE` | At least `max(1,total_no_links)`. |
!> | `NXEE` | At least `NX`. |
!> | `NOCTAB` | At least 1. |
!> | `NXSCEE` | Greater than 1 for channel cross-section lookup tables. |
!> | `total_no_links` | Non-negative and less than `total_no_elements`. |
!> | `NX`, `NY` | Both at least 1. |
!> | `NGDBGN` | Equal to `total_no_links + 1`. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-01-30 | RAH | 4.2 | Created this routine. |
!> | 2009-01 | JE | - | Removed the `NELEE >= NOCTAB*NOCTAB` restriction. |
!> @endhistory
   SUBROUTINE OCCHK0()
      INTEGER       :: ERRNUM, I, IUNIT, NERR, OUNIT
      INTEGER, PARAMETER :: IUNDEF = 0
      INTEGER       :: IDUMS(1), IDUMO(1)
      LOGICAL       :: BOPEN, LDUM1(1)
      CHARACTER(47) :: MSG
      CHARACTER(11) :: FORM
      CHARACTER(3)  :: NAME
      NERR = 0
      !----------------------------------------------------------------------*
      ! 1. Unit Numbers
      ! ---------------
      ! PRI, OCD
      OUNIT = PPPRI
      IUNIT = PPPRI
      NAME = 'PRI'

      DO I = 0, 1
         INQUIRE (IUNIT, OPENED=BOPEN, FORM=FORM)

         IF (.NOT. BOPEN) THEN
            WRITE (MSG, '("File unit ",A," =",I4,1X,A)') NAME, IUNIT, 'is not connected to a file'
            ERRNUM = 1008
            IF (I == 0) OUNIT = 0
            CALL ERROR(EEERR, ERRNUM, OUNIT, 0, 0, MSG)
            NERR = NERR + 1
         ELSE IF (FORM /= 'FORMATTED') THEN
            WRITE (MSG, '("File unit ",A," =",I4,1X,A,1X,A)') NAME, IUNIT, 'has format type', FORM
            ERRNUM = 1009
            IF (I == 0) OUNIT = 0
            CALL ERROR(EEERR, ERRNUM, OUNIT, 0, 0, MSG)
            NERR = NERR + 1
         END IF

         ! Setup for the next iteration (I=1)
         IUNIT = OCD
         NAME = 'OCD'
      END DO

      IDUMS(1) = MIN(PPPRI, OCD)

      CALL ALCHKI(EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ PRI, OCD ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)

      ! 2. Array Sizes
      ! --------------
      ! NELEE
      IDUMS(1) = NELEE
      IDUMO(1) = MAX(NX, total_no_elements)! , NOCTAB*NOCTAB)
      CALL ALCHKI(EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      ! NLFEE
      IDUMS(1) = NLFEE
      IDUMO(1) = MAX(1, total_no_links)
      CALL ALCHKI(EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      ! NXEE
      IDUMS(1) = NXEE
      IDUMO(1) = NX
      CALL ALCHKI(EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      ! NOCTAB
      IDUMS(1) = NOCTAB
      CALL ALCHKI(EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NOCTAB', 'GE', IONE1, IDUMS, NERR, LDUM1)
      ! NXSCEE
      IDUMS(1) = NXSCEE

      CALL ALCHKI(EEERR, 1002, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXSCEE', 'GT', IONE1, IDUMS, NERR, LDUM1)

      ! 3. Number of Entities
      ! ---------------------
      ! NLF
      IDUMS(1) = total_no_links
      CALL ALCHKI(EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', IZERO1, IDUMS, NERR, LDUM1)
      IDUMO(1) = total_no_elements
      CALL ALCHKI(EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', IDUMO, IDUMS, NERR, LDUM1)
      ! NX, NY
      IDUMS(1) = MIN(NX, NY)
      CALL ALCHKI(EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ NX, NY ]', 'GE', IONE1, IDUMS, NERR, LDUM1)
      ! NGDBGN
      IDUMS(1) = NGDBGN
      IDUMO(1) = total_no_links + 1

      CALL ALCHKI(EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NGDBGN', 'EQ', IDUMO, IDUMS, NERR, LDUM1)

      ! 4. Finish
      ! ---------
      IF (NERR > 0) THEN
         CALL ERROR(FFFATAL, 1000, OUNIT, 0, 0, 'Error(s) detected while checking OC input variables & constants')
      END IF

   END SUBROUTINE OCCHK0

!> @brief Checks static OC topology and channel-definition arrays.
!>
!> `OCCHK1` verifies neighbour references, active-grid indexing, and the
!> link-code grids used to locate north-south and east-west channel links.
!> Positive neighbour and grid references must not exceed
!> `total_no_elements`. Any `LCODEX` or `LCODEY` value in the
!> channel-boundary range 7:11 must map through `LINKNO` to a valid
!> channel-link element, i.e. an index greater than zero and less than
!> `NGDBGN`.
!>
!> Entry requirements:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NEL >= 1`, `NX >= 1`, `NY >= 1` | The element and grid dimensions are populated. |
!> | `NELEE >= NEL`, `NXEE >= NX` | Workspace leading dimensions cover the active model extent. |
!> | `PRI` open for formatted output | Error reporting can write diagnostics. |
!> | `size_of_LDUM1 >= max(NX,NEL)` | Logical workspace is large enough for the largest check in this routine. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-03 | RAH | 4.2 | Created this routine. |
!> | 1998-02-05 | RAH | 4.2 | Added the `LDUM1` argument. |
!> @endhistory
   SUBROUTINE OCCHK1(SZLOG, LDUM1)

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN)  :: SZLOG        !! Size of the logical check-result workspace `LDUM1`.
      LOGICAL, INTENT(OUT) :: LDUM1(SZLOG) !! Discarded per-entry check-result scratch.

      ! Locals
      INTEGER :: CODE, FACE, I, IELx, X, Y, TYPEE
      INTEGER :: NERR, IUNDEF
      INTEGER :: IDUMO(1)

      CHARACTER(LEN=23) :: NAME
      CHARACTER, PARAMETER :: XY(0:1) = ['X', 'Y']

      !----------------------------------------------------------------------*

      ! Initialize local variables in the executable block to avoid implicit SAVE bugs
      NERR = 0
      IUNDEF = 0
      NAME = 'validity_of_LCODE?(x,y)'

      ! 1. Index Arrays
      ! ---------------
      IDUMO(1) = total_no_elements

      ! ICMREF
      face_loop: DO FACE = 1, 4
         CALL ALCHKI(EEERR, 1057, PPPRI, 1, total_no_elements, FACE, 2, 'ICMREF(iel,face,2)', &
                     'LE', IDUMO, ICMREF(1:total_no_elements, 4 + FACE), NERR, LDUM1(1:total_no_elements))
      END DO face_loop

      ! ICMXY
      y_icmxy_loop: DO Y = 1, NY
         ! Modernized: Passing explicit array slice ICMXY(1:NX, Y) instead of scalar start point
         CALL ALCHKI(EEERR, 1057, PPPRI, 1, NX, Y, IUNDEF, 'ICMXY(x,y)', &
                     'LE', IDUMO, ICMXY(1:NX, Y), NERR, LDUM1(1:NX))
      END DO y_icmxy_loop

      ! 2. Channel Definition Arrays
      ! ----------------------------
      ! LCODEX, LCODEY
      xy_loop: DO I = 0, 1

         ! Inject 'X' or 'Y' into the 18th character of the string
         NAME(18:18) = XY(I)

         y_lcode_loop: DO Y = 1, NY
            x_lcode_loop: DO X = 1, NX
               CODE = 0
               TYPEE = LCODEX(X, Y)*(1 - I) + LCODEY(X, Y)*I

               IF (TYPEE >= 7 .AND. TYPEE <= 11) THEN
                  IELx = LINKNO(X, Y, I == 0)
                  IF (IELx <= 0 .OR. IELx >= NGDBGN) CODE = TYPEE
               END IF

               IDUM(X) = CODE
            END DO x_lcode_loop

            ! Modernized: Explicit array slice for IDUM
            CALL ALCHKI(EEERR, 1058, PPPRI, 1, NX, Y, IUNDEF, NAME, 'EQ', &
                        IZERO1, IDUM(1:NX), NERR, LDUM1(1:NX))
         END DO y_lcode_loop

      END DO xy_loop

      ! 3. Finish
      ! ---------
      IF (NERR > 0) THEN
         CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking static OC input arrays')
      END IF

   END SUBROUTINE OCCHK1

!> @brief Checks OC input values after [[OCREAD]].
!>
!> The checks cover boundary file units, overland/channel roughness values,
!> and channel cross-section tables, including monotonic level coordinates
!> and positive final widths.
!>
!> Boundary files `OHB` and `OFB` are checked only when their corresponding
!> boundary counts are non-zero. Roughness checks still call `ALCHK` with a
!> positive-roughness test for both `STRXX` and `STRYY`; however, the final
!> response to accumulated errors is a warning rather than a fatal error.
!> This preserves the current surface-storage convention where negative
!> `STRXX` values can be passed through to [[ocqdqmod:ocqdq]].
!>
!> Channel cross-section checks require the first depth to be zero, depth
!> values to be strictly increasing, widths to be non-decreasing, and the
!> final width for each active link to be positive.
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `total_no_elements >= max(total_no_links,1)` | Active element range covers active links. |
!> | `NLFEE >= max(total_no_links,1)` | Link-indexed arrays cover active links. |
!> | `PRI` open for formatted output | Error reporting can write diagnostics. |
!>
!> @warning
!> `LDUM1` is declared `INTENT(INOUT)` here (the legacy routine declared it
!> `INTENT(IN)`) so it can be passed as the check-result buffer to
!> `ALCHK`/`ALCHKI`; the `USE CONST_SY` import is unused in this routine's
!> current body.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-03 | RAH | 4.2 | Created this routine, taking part of it from [[OCPLF]]. |
!> | 1998-02-06 | RAH | 4.2 | Added the boundary-file unit checks. |
!> | 1998-02-18 | RAH | 4.2 | Skipped the unit checks when `NONEED` is true. |
!> | 2022-05-19 | SB | - | Demoted the final error response from fatal to a warning, allowing the negative-`STRXX` surface-storage marker to pass through. |
!> @endhistory
   SUBROUTINE OCCHK2(DDUM1A, DDUM1B, SZLOG, LDUM1)

      USE CONST_SY

      IMPLICIT NONE

      INTEGER, INTENT(IN)           :: SZLOG          !! Size of the logical check-result workspace `LDUM1`.
      DOUBLE PRECISION, INTENT(OUT) :: DDUM1A(:)      !! Discarded cross-section lower-bound scratch.
      DOUBLE PRECISION, INTENT(OUT) :: DDUM1B(:)       !! Discarded cross-section upper-bound scratch.
      LOGICAL, INTENT(INOUT)        :: LDUM1(SZLOG)    !! Discarded per-entry check-result scratch; see the routine's warning.

      INTEGER :: ERRNUM, I, IELw, IUNDEF, IUNIT, N, NERR
      INTEGER :: IDUMS(1)
      LOGICAL :: BOPEN, NONEED
      CHARACTER(47) :: MSG
      CHARACTER(11)  :: FORM
      CHARACTER(3)   :: NAME
      CHARACTER(19)  :: SUBJ

      !----------------------------------------------------------------------*

      NERR = 0
      IUNDEF = 0

      ! 1. Unit Numbers
      ! ---------------
      ! OHB, OFB
      IDUMS(1) = 0
      IUNIT = OHB
      NAME = 'OHB'
      NONEED = NOCHB == 0

      DO I = 0, 1
         IF (.NOT. NONEED) THEN
            IDUMS(1) = MIN(IUNIT, IDUMS(1))
            INQUIRE (IUNIT, OPENED=BOPEN, FORM=FORM)

            IF (.NOT. BOPEN) THEN
               WRITE (MSG, 9100) NAME, IUNIT, 'is not connected to a file'
               ERRNUM = 1008
               CALL ERROR(EEERR, ERRNUM, PPPRI, 0, 0, MSG)
               NERR = NERR + 1
            ELSE IF (FORM /= 'FORMATTED') THEN
               WRITE (MSG, 9100) NAME, IUNIT, 'has format type', FORM
               ERRNUM = 1009
               CALL ERROR(EEERR, ERRNUM, PPPRI, 0, 0, MSG)
               NERR = NERR + 1
            END IF
         END IF

         ! Setup for OFB on the second pass
         IUNIT = OFB
         NAME = 'OFB'
         NONEED = NOCFB == 0
      END DO

      CALL ALCHKI(EEERR, 1003, PPPRI, 1, 1, IUNDEF, IUNDEF, '[ OHB, OFB ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)

      ! 2. Element Properties
      ! ---------------------
      ! STRX
      CALL ALCHK(EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRX(iel)', 'GT', ZERO1, ZERO, STRXX, NERR, LDUM1)
      ! STRY
      CALL ALCHK(EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRY(iel)', 'GT', ZERO1, ZERO, STRYY, NERR, LDUM1)

      ! 3. Cross-section Tables
      ! -----------------------
      !
      IF (total_no_links > 0) THEN
         ! XINH
         CALL ALCHK(EEERR, 1016, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINH(link)[j=1]', 'EQ', ZERO1, ZERO, XINH, NERR, LDUM1)

         DO IELw = 1, total_no_links
            N = NXSECT(IELw) - 1
            WRITE (SUBJ, 9310) IELw

            DDUM1A(1:N) = XINH(IELw, 1:N)
            DDUM1B(1:N) = XINH(IELw, 2:N + 1)
            CALL ALCHK(EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, 'GTa', DDUM1A, ZERO, DDUM1B, NERR, LDUM1)

            ! XINW
            SUBJ(4:4) = 'W'
            DDUM1A(1:N) = XINW(IELw, 1:N)
            DDUM1B(1:N) = XINW(IELw, 2:N + 1)
            CALL ALCHK(EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, 'GEa', DDUM1A, ZERO, DDUM1B, NERR, LDUM1)
         END DO

         DO IELw = 1, total_no_links
            DDUM1A(IELw) = XINW(IELw, NXSECT(IELw))
         END DO

         CALL ALCHK(EEERR, 1056, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINW[link,NXSECT(link)]', 'GT', ZERO1, ZERO, &
                    DDUM1A, NERR, LDUM1)
      END IF

      IF (NERR > 0) THEN
         ! sb 190522 negative strickler for surface storage
         CALL ERROR(WWWARN, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
         ! CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
      END IF

      ! Format Statements
      ! -----------------
9100  FORMAT('File unit ', A, ' =', I4, 1X, A:1X, A)
9310  FORMAT('XINH[ link =', I3, '](j)')

   END SUBROUTINE OCCHK2

!> @brief Reads time-varying head and flux boundary values for the current OC step.
!>
!> Boundary time series are advanced from the head-boundary and flux-boundary
!> files into `HOCNOW` and `QOCF`; end-of-file markers are treated as fatal
!> input errors.
!>
!> `HINPUT` interpolates or advances head values for `NOCHB` categories using
!> `TIH`, `OCNOW`, and `OCNEXT`. `FINPUT` does the same for `NOCFB` flux
!> categories. The resulting `QOCF` values are prescribed inflow rates
!> consumed by [[ocmod2:ocqbc]].
!>
!> @warning
!> [[OCINI]] does not explicitly initialise `HOCLST`, `HOCNXT`, `QFLAST`,
!> `QFNEXT`, `HOCPRV`, `HOCNXV`, or `QOCFIN` before the first call to this
!> routine, so `HINPUT`/`FINPUT` receive processor-dependent initial state on
!> that call. This documents current behaviour; it was not repaired in this
!> transfer.
!> @endwarning
   SUBROUTINE OCEXT

      IMPLICIT NONE

      !----------------------------------------------------------------------*

      ! --- HEAD BOUNDARY ---
      IF (NOCHB > 0) THEN
         CALL HINPUT(OHB, TIH, OCNOW, OCNEXT, HOCLST, HOCNXT, &
                     HOCPRV(1:NOCHB), HOCNXV(1:NOCHB), NOCHB, HOCNOW(1:NOCHB))
      END IF

      IF (EQMARKER(HOCNXT)) THEN
         CALL ERROR(FFFATAL, 1007, PPPRI, 0, 0, 'END OF OC HEAD BOUNDARY DATA')
      END IF

      ! --- FLUX BOUNDARY ---
      IF (NOCFB > 0) THEN
         CALL FINPUT(OFB, TIH, OCNOW, OCNEXT, QFLAST, QFNEXT, &
                     QOCFIN(1:NOCFB), NOCFB, QOCF(1:NOCFB))
      END IF

      IF (EQMARKER(QFNEXT)) THEN
         CALL ERROR(FFFATAL, 1023, PPPRI, 0, 0, 'END OF OC FLUX BOUNDARY DATA')
      END IF

   END SUBROUTINE OCEXT

!> @brief Builds row-order indexing for the implicit OC solver.
!>
!> The catchment is split into y-coordinate rows, with west/south channel
!> links and optional bank elements inserted beside the grid elements. The
!> resulting `NROWST`, `NROWEL`, and `NELIND` arrays define the block rows
!> used by [[OCSIM]].
!>
!> Rows follow the basic grid `y` coordinate. East-west links and their banks
!> are included in the row above the link, matching the legacy OC row solver
!> ordering.
!>
!> | Array | Meaning |
!> |:------|:--------|
!> | `NROWF` | First non-empty row number. |
!> | `NROWL` | Last non-empty row number. |
!> | `NROWST(j)` | Pointer into `NROWEL` for the first element in row `j`. |
!> | `NROWEL` | Contiguous list of elements in row order. |
!> | `NELIND(e)` | Position of element `e` within its row. |
!>
!> If element `i` is the `p`th entry in row `j`, then
!>
!> \[
!> e = NROWEL(NROWST(j)+p-1),
!> \]
!>
!> and `NELIND` is the partial inverse:
!>
!> \[
!> NELIND\left(NROWEL(NROWST(j)+p-1)\right)=p.
!> \]
!>
!> The row of grid element `ICMXY(x,y)`, and of any associated link/bank
!> elements inserted while processing that grid square, is `y`.
!>
!> For each grid square `(i,j)`, `OCIND` scans the west face (`FACE=3`) and
!> then the south face (`FACE=4`). If a link is present, the row receives
!>
!> | Bank option | Inserted sequence |
!> |:------------|:------------------|
!> | `BEXBK=.FALSE.` | `link` |
!> | `BEXBK=.TRUE.` | bank on one side, `link`, bank on the other side |
!>
!> using `ICMBK(link,5-FACE)` before the link and `ICMBK(link,FACE-2)` after
!> the link. On the west-face pass only, the active grid element
!> `ICMXY(i,j)` is then inserted. Thus the current row length is
!>
!> \[
!> n_j = NROWST(j+1)-NROWST(j),
!> \]
!>
!> and the maximum row width checked against `NXOCEE` is
!>
!> \[
!> NXOC = \max_j n_j.
!> \]
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NLFEE >= max(total_no_links,1)` | Link-indexed arrays cover the active link set. |
!> | `NXEE >= max(NX,1)` and `NY >= 1` | Grid-indexed arrays cover the active grid. |
!> | `LINKNO` on west and south faces is at most `total_no_links` | Link lookup stays within the defined `ICMBK` extent. |
!> | `1 <= ICMBK(1:total_no_links,1:2) <= total_no_elements` when banks are active | Bank elements can be indexed in `NELIND`/`NROWEL`. |
!> | Active grid elements, active links, and optional bank elements partition `1:total_no_elements` | Every OC element appears exactly once in row order. |
   SUBROUTINE OCIND(BEXBK, NROWF, NROWL, NROWST, NELIND, NROWEL)

      IMPLICIT NONE

      ! Arguments
      LOGICAL, INTENT(IN)  :: BEXBK        !! True when explicit bank elements are inserted beside their links.
      INTEGER, INTENT(OUT) :: NROWF        !! First non-empty row number.
      INTEGER, INTENT(OUT) :: NROWL        !! Last non-empty row number.
      INTEGER, INTENT(OUT) :: NROWST(NY + 1) !! Row-start pointer into `NROWEL`.
      INTEGER, INTENT(OUT) :: NELIND(:)    !! Position of each element within its row.
      INTEGER, INTENT(OUT) :: NROWEL(:)    !! Contiguous list of elements in row order.

      ! Locals
      INTEGER :: BANK, FACE, I, ICOUNT, IELv, J, K, LINK, NXOC

      !----------------------------------------------------------------------*

      ! Initialize counters
      NXOC = 0
      K = 0

      ! LOOP OVER BASIC GRID SYSTEM
      ! - LOOP OVER EACH ROW

      row_loop: DO J = 1, NY
         NROWST(J) = K + 1
         IF (K == 0) NROWF = J

         ! ---- LOOP OVER EACH GRID SQUARE IN ROW
         ICOUNT = 0

         col_loop: DO I = 1, NX

            ! ------- Loop over west & south faces
            face_loop: DO FACE = 3, 4

               ! ---------- Test for link at face of grid
               LINK = LINKNO(I, J, FACE == 3)

               IF (LINK > 0) THEN
                  IF (BEXBK) THEN
                     BANK = ICMBK(LINK, 5 - FACE)
                     K = K + 1
                     ICOUNT = ICOUNT + 1
                     NROWEL(K) = BANK
                     NELIND(BANK) = ICOUNT
                  END IF

                  K = K + 1
                  ICOUNT = ICOUNT + 1
                  NROWEL(K) = LINK
                  NELIND(LINK) = ICOUNT

                  IF (BEXBK) THEN
                     BANK = ICMBK(LINK, FACE - 2)
                     K = K + 1
                     ICOUNT = ICOUNT + 1
                     NROWEL(K) = BANK
                     NELIND(BANK) = ICOUNT
                  END IF
               END IF

               ! ---------- Test for active grid square
               IF (FACE == 3) THEN
                  IELv = ICMXY(I, J)
                  IF (IELv > 0) THEN
                     K = K + 1
                     ICOUNT = ICOUNT + 1
                     NROWEL(K) = IELv
                     NELIND(IELv) = ICOUNT
                  END IF
               END IF

            END DO face_loop
         END DO col_loop

         ! ---- Next row
         NXOC = MAX(NXOC, K + 1 - NROWST(J))
         IF (ICOUNT > 0) NROWL = J

      END DO row_loop

      ! - This marks the end of the last row (+1)
      ! Modern Fix: Explicitly use NY + 1 instead of relying on the leaked loop variable 'J'
      NROWST(NY + 1) = K + 1

      ! CHECK ARRAY DIMENSIONS
      IF (NXOC > NXOCEE) THEN
         CALL ERROR(FFFATAL, 1006, PPPRI, 0, 0, 'ARRAY DIMENSION OF NXOC TOO SMALL')
      END IF

   END SUBROUTINE OCIND

!> @brief Reads an alphanumeric channel-definition grid.
!>
!> `OCLTL` decodes the legacy one-character OC map into integer link and
!> boundary codes, preserving row-number checks and optional echo printing.
!> Input rows must be supplied from `NNY` down to 1; an unexpected row number
!> prints an "incorrect coordinate" marker when echo output is enabled and
!> then stops the program.
!>
!> Character mapping:
!>
!> | Character | Code | Meaning in OC flow-code grids |
!> |:----------|:-----|:-------------------------------|
!> | `I` | 1 | Internal impermeable boundary. |
!> | `.` | 2 | No special OC boundary/link code. |
!> | `R` | 6 | River/channel link without boundary type. |
!> | `W` | 7 | Channel weir boundary. |
!> | `A` | 8 | Channel river/resistance plus weir boundary. |
!> | `H` | 9 | Channel time-varying head boundary. |
!> | `F` | 10 | Channel time-varying flow boundary. |
!> | `P` | 11 | Channel polynomial boundary. |
!>
!> Characters not listed in `CODES` leave the target entry at its initial
!> zero value.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-12 | - | - | Created this routine. |
!> | 2015-04-21 | SB | - | Increased the `A1LINE` row buffer and its read/write format from 200 to 500 characters for larger catchments. |
!> @endhistory
   SUBROUTINE OCLTL(NNX, NNY, IARR, NXE, NYE, INF, IOF, BPCNTL)
      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)  :: NNX    !! X dimension of the grid to read.
      INTEGER, INTENT(IN)  :: NNY    !! Y dimension of the grid to read.
      INTEGER, INTENT(IN)  :: NXE    !! First declared extent of `IARR`.
      INTEGER, INTENT(IN)  :: NYE    !! Second declared extent of `IARR`.
      INTEGER, INTENT(IN)  :: INF    !! Input file unit for the OC map records.
      INTEGER, INTENT(IN)  :: IOF    !! Echo-output file unit.
      INTEGER, INTENT(OUT) :: IARR(NXE, NYE) !! Decoded OC flow-code grid; entries within `1:NNX,1:NNY` are overwritten.
      LOGICAL, INTENT(IN)  :: BPCNTL !! Enables echo printing and coordinate-error output.

      ! Local Variables
      CHARACTER(LEN=80)    :: TITLE
      CHARACTER(LEN=1)     :: A1LINE(500)
      INTEGER              :: I, J, K, L, M

      CHARACTER(LEN=1), PARAMETER :: CODES(11) = &
                                     ['I', '.', ' ', ' ', ' ', 'R', 'W', 'A', 'H', 'F', 'P']

      READ (INF, '(A80)') TITLE
      IF (BPCNTL) WRITE (IOF, '(A80)') TITLE

      IARR(1:NNX, 1:NNY) = 0

      I = NNY

      read_loop: DO J = 1, NNY
         READ (INF, '(I7, 1X, 500A1)') K, A1LINE(1:NNX)
         IF (BPCNTL) WRITE (IOF, '(I7, 1X, 500A1)') K, A1LINE(1:NNX)

         IF (K /= I) THEN
            IF (BPCNTL) WRITE (IOF, "('  ^^^   INCORRECT COORDINATE')")
            STOP 'INCORRECT COORDINATE'
         END IF

         I = I - 1

         line_loop: DO L = 1, NNX
            search_code: DO M = 1, 11
               IF (A1LINE(L) == CODES(M) .AND. CODES(M) /= ' ') THEN
                  IARR(L, K) = M
                  EXIT search_code
               END IF
            END DO search_code
         END DO line_loop

      END DO read_loop

   END SUBROUTINE OCLTL

!> @brief Reads per-link channel geometry and link boundary data.
!>
!> `OCPLF` reads default and explicit cross-section definitions, link bed
!> elevations, initial water depths, Strickler roughness coefficients, and
!> boundary-condition parameters for river-link boundary types.
!>
!> Channel data follow the manual records `OC30`-`OC41`. The cross-section
!> selector `IDEFX` on `OC36` is interpreted as:
!>
!> | `IDEFX` value | Meaning |
!> |:--------------|:--------|
!> | `< 0` | Use default cross-section category `-IDEFX` from records `OC32`-`OC34`. |
!> | `> 0` and not 1 | Read `IDEFX` width/depth pairs from following record `OC37`. |
!> | `0`, `1`, `< -NDEFCT`, or `> NOCTAB` | Invalid; increments `IXER`, and very large positive values stop further link processing. |
!>
!> For each link `iel`, `OCPLF` stores bed elevation in `ZGRUND(iel)`,
!> initial water surface as `SETHRF(iel,ZGRUND+WDEPTH)`, link roughness in
!> both `STRXX(iel)` and `STRYY(iel)`, the active cross-section count in
!> `NXSECT(iel)`, bankfull width in `CWIDTH(iel)`, and bankfull elevation in
!> `ZBFULL(iel)`.
!>
!> Boundary-specific records appended after each link are:
!>
!> | Type | Record | Stored values |
!> |:-----|:-------|:--------------|
!> | 7 or 8 | `OC38` | `IFACE`, `COEFF`, `SUBRIO`, `ZSILL`, `ZL`; category is set to 1. |
!> | 9 | `OC39` | Time-varying head category; face is set to 0. |
!> | 10 | `OC40` | Boundary face and time-varying flow category. |
!> | 11 | `OC41` | Boundary face and five polynomial coefficients; category is set to 1. |
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NLFEE >= total_no_links` and `total_no_links >= 1` | Link-indexed arrays cover active links. |
!> | `NOCTAB >= 1` and `NOCBCC(1:total_no_links) <= NOCTAB` | Boundary indices fit the OC boundary table. |
!> | `OCD` open for formatted input | Per-link channel data can be read. |
!> | `PRI` open for formatted output | Diagnostics can be written. |
!>
!> Exit conditions retained from the legacy routine are:
!>
!> | Condition | Meaning |
!> |:----------|:--------|
!> | `IXER(out) >= IXER(in)` | Input-error count is monotonic. |
!> | `IXER(out) == IXER(in)` implies `2 <= NXSECT(1:total_no_links) <= NOCTAB` | Each valid link has a usable cross-section table. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-01-21 | RAH | 4.2 | Created this routine, fixing an error in the second `COCBCD` subscript. |
!> | 1998-02-03 | RAH | 4.2 | Moved cross-section table set-up to the new [[OCXS]] and value checks to the new [[OCCHK2]]. |
!> | 2009-01 | JE | - | Restructured the read loop for automatic differentiation. |
!> @endhistory
   SUBROUTINE OCPLF(BOUT, IXER, fromNOCBCD, NXDEF, XDEFW)

      IMPLICIT NONE

      LOGICAL, INTENT(INOUT) :: BOUT   !! True to echo link data to `PRI`.
      INTEGER, INTENT(INOUT) :: IXER   !! OC input-error count; only ever increased here.
      INTEGER, INTENT(INOUT) :: fromNOCBCD(NOCTAB, 2:4) !! Boundary-face/category columns of `NOCBCD`, updated for river-link boundary types.
      INTEGER, INTENT(OUT)   :: NXDEF(NOCTAB) !! Number of width/depth pairs in each default cross-section category.
      DOUBLE PRECISION       :: XDEFH(NOCTAB, NOCTAB) !! Default cross-section depths by category.
      DOUBLE PRECISION       :: XDEFW(NOCTAB, NOCTAB) !! Default cross-section widths by category.

      INTEGER :: I, IBC, IDEF, IDEFX, ielm, J, N, NDEFCT, TYPEE, ios
      DOUBLE PRECISION :: STR, WDEPTH, ZG
      LOGICAL :: TEST, g8055, g8013, g8300, greturn
      CHARACTER(102) :: MSG

      !----------------------------------------------------------------------*
      !
      ! READ DEFAULT CHANNEL CROSS-SECTIONS
      ! :OC30

      READ (OCD, *)
      READ (OCD, *) NDEFCT

      IF ((NDEFCT > NOCTAB) .OR. (NDEFCT < 0)) THEN
         WRITE (MSG, 9054) NDEFCT, NOCTAB
         CALL ERROR(EEERR, 1054, PPPRI, 0, 0, MSG)
         IXER = IXER + 1
      END IF

      g8013 = .FALSE.
      g8055 = .FALSE.
      g8300 = .FALSE.
      greturn = .FALSE.

      ! :OC32
      IF (NDEFCT > 0) THEN
         READ (OCD, *)
         IF (BOUT) WRITE (PPPRI, 9032) 'Category', 'Width', 'Height'

         out100: DO IDEF = 1, NDEFCT
            IF (g8055) CYCLE out100
            READ (OCD, *) N

            IF ((N > NOCTAB) .OR. (N < 2)) THEN
               g8055 = .TRUE.
               CYCLE out100
            END IF

            NXDEF(IDEF) = N
            READ (OCD, *) (XDEFW(IDEF, J), XDEFH(IDEF, J), J=1, N)

            IF (BOUT) WRITE (PPPRI, 9034) IDEF, (XDEFW(IDEF, J), XDEFH(IDEF, J), J=1, N)
         END DO out100
      END IF

      !
      ! READ DATA FOR EACH LINK
      ! :OC35
      IF (g8055) THEN
         WRITE (MSG, 9055) IDEF, N, NOCTAB
         CALL ERROR(EEERR, 1055, PPPRI, 0, 0, MSG)
         IXER = IXER + 1
      ELSE
         READ (OCD, *)
         IF (BOUT) WRITE (PPPRI, 9035) 'Element', 'Elevation', 'Init.Depth', 'Strickler', 'Width', 'Height'

         out500: DO ielm = 1, total_no_links
            IF (g8013 .OR. g8300 .OR. greturn) CYCLE out500

            ! Modernized with IOSTAT check
            READ (OCD, *, IOSTAT=ios) I, ZG, WDEPTH, STR, IDEFX

            IF (ios /= 0) THEN
               g8300 = .TRUE.
               CYCLE out500
            END IF

            IF (I /= ielm) THEN
               g8013 = .TRUE.
               CYCLE out500
            END IF

            ZGRUND(ielm) = ZG
            CALL SETHRF(ielm, ZG + WDEPTH)
            STRXX(ielm) = STR
            STRYY(ielm) = STR

            ! :OC37
            TEST = (IDEFX == 1) .OR. (IDEFX > NOCTAB)

            IF ((IDEFX == 0) .OR. (IDEFX < -NDEFCT) .OR. TEST) THEN
               WRITE (MSG, 9012) IDEFX, -NDEFCT, NOCTAB
               CALL ERROR(EEERR, 1012, PPPRI, ielm, 0, MSG)
               IXER = IXER + 1

               IF (TEST) THEN
                  greturn = .TRUE.
                  CYCLE out500
               END IF

            ELSE
               IF (IDEFX > 0) THEN
                  N = IDEFX
                  READ (OCD, *) (XINW(ielm, J), XINH(ielm, J), J=1, N)
                  IF (BOUT) WRITE (PPPRI, 9037) ielm, ZG, WDEPTH, STR, (XINW(ielm, J), XINH(ielm, J), J=1, N)
               ELSE
                  IDEF = -IDEFX
                  N = NXDEF(IDEF)
                  ! Native Fortran array slice copying N elements
                  XINH(ielm, 1:N) = XDEFH(IDEF, 1:N)
                  XINW(ielm, 1:N) = XDEFW(IDEF, 1:N)
                  IF (BOUT) WRITE (PPPRI, 9137) ielm, ZG, WDEPTH, STR, IDEF
               END IF

               NXSECT(ielm) = N

               ! CHANNEL BANK-FULL WIDTH & ELEVATION
               CWIDTH(ielm) = XINW(ielm, N)
               ZBFULL(ielm) = XINH(ielm, N) + ZG
            END IF

            ! READ IN ADDITIONAL DATA FOR BOUNDARY CONDITIONS
            ! :OC38-41
            IBC = NOCBCC(ielm)

            IF (IBC > 0) THEN
               TYPEE = fromNOCBCD(IBC, 3)

               IF ((TYPEE == 7) .OR. (TYPEE == 8)) THEN
                  READ (OCD, *) fromNOCBCD(IBC, 2), (COCBCD(J, IBC), J=1, 4)
                  fromNOCBCD(IBC, 4) = 1
               ELSE IF (TYPEE == 9) THEN
                  fromNOCBCD(IBC, 2) = 0
                  READ (OCD, *) fromNOCBCD(IBC, 4)
               ELSE IF (TYPEE == 10) THEN
                  READ (OCD, *) (fromNOCBCD(IBC, J), J=2, 4, 2)
               ELSE IF (TYPEE == 11) THEN
                  READ (OCD, *) fromNOCBCD(IBC, 2), (COCBCD(J, IBC), J=1, 5)
                  fromNOCBCD(IBC, 4) = 1
               END IF
            END IF

         END DO out500
      END IF

      ! Epilogue Error Catching
      IF (greturn) THEN
         RETURN
      ELSE IF (g8013) THEN
         WRITE (MSG, 9013) ielm, I
         CALL ERROR(EEERR, 1013, PPPRI, ielm, 0, MSG)
         IXER = IXER + 1
      ELSE IF (g8300) THEN
         MSG = 'Channel input data is missing or has incorrect format'
         CALL ERROR(EEERR, 1019, PPPRI, ielm, 0, MSG)
         IXER = IXER + 1
      END IF

      ! Format Statements
9012  FORMAT('Cross-section number IDEFX =', I4, ' lies outside ranges', &
             ' -NDEFCT:-1 =', I4, ' : -1  and  2:NOCTAB = 2 :', I4)

9013  FORMAT('Expected element number,', I5, ', but found', I5, ', ', &
             'while reading channel data')

9032  FORMAT(/5X, 'Default Channel Cross-sections:'//5X, 3A10/)

9034  FORMAT(5X, I10, (T16, 2F10.3))

9035  FORMAT(/5X, 'Link Element Data:'//5X, 6A11/)

9037  FORMAT(5X, I11, 3F11.3, (T50, 2F11.3))

9054  FORMAT('Number of default channel cross-section categories ', &
             'NDEFCT =', I4, 2X, 'lies outside range 0:NOCTAB = 0 :', I4)

9055  FORMAT('Number of width/elevation pairs NXDEF(', I3, ') =', I4, 2X, &
             'lies outside range 2:NOCTAB = 2:', I4)

9137  FORMAT(5X, I11, 3F11.3, 3X, 'default category', I3)

   END SUBROUTINE OCPLF

!> @brief Prints one OC diagnostic block to the main print file.
!>
!> The report is written only when [[OCSIM]] calls this routine for an
!> output time in the requested interval. It lists each element, the four
!> `QOC` face flows after conversion to the model x/y sign convention, and
!> the current water level `HRF`.
!>
!> | Element range | Extra field |
!> |:--------------|:------------|
!> | `1:total_no_links` | `ARXL`, the current channel wetted area. |
!> | `total_no_links+1:total_no_elements` | No channel area is printed. |
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NELEE >= total_no_elements` and `total_no_elements >= max(total_no_links,1)` | Element arrays cover active elements and links. |
!> | `total_no_links >= 0` and `total_no_links <= size(ARXL)` | Channel area values are available for printed links. |
!> | `PRI` open for formatted output | The report can be written. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-26 | RAH | 4.2 | Created this routine. |
!> @endhistory
   SUBROUTINE OCPRI(OCNOW, ARXL, QOC)
      DOUBLEPRECISION, INTENT(IN) :: OCNOW      !! Simulation time being reported, in hours.
      DOUBLEPRECISION, INTENT(IN) :: ARXL(:)    !! Current channel wetted cross-sectional area, by link.
      DOUBLEPRECISION, INTENT(IN) :: QOC(NELEE, 4) !! Current face flows in the model x/y sign convention.
      DOUBLEPRECISION, ALLOCATABLE :: ghrf(:)   !! Local copy of the current water level, by link.
      INTEGER                     :: FACE, ielmm
!----------------------------------------------------------------------*
      ALLOCATE (ghrf(total_no_links))

      WRITE (PPPRI, 9100) 'AFTER', OCNOW, ' HOURS ----'
      WRITE (PPPRI, 9200) 'iel', ('QOC(iel,', FACE, ')', FACE=1, 4), 'HRF', 'ARXL'
      DO ielmm = 1, total_no_links
         ghrf(ielmm) = GETHRF(ielmm)
      END DO
      WRITE (PPPRI, 9210) (ielmm, (QOC(ielmm, FACE), FACE=1, 4), ghrf(ielmm), ARXL(ielmm), ielmm=1, total_no_links)
      DO ielmm = total_no_links + 1, total_no_elements
         WRITE (PPPRI, 9210) ielmm, (QOC(ielmm, FACE), FACE=1, 4), GETHRF(ielmm)
      END DO

      WRITE (PPPRI, 9100) 'END ----'
9100  FORMAT(//'---- OC MODULE  RESULTS ', A:F10.2, A//)
9200  FORMAT(4X, A4, 4(2X, A8, I1, A1), 2A12/)

9210  FORMAT(4X, I4, SP, 4F12.3, S, 2F12.3)
   END SUBROUTINE OCPRI

!> @brief Reads and dispatches the static OC input file.
!>
!> `OCREAD` loads timestep/output controls, roughness parameters, initial
!> overland water depths, boundary-condition definitions, and channel-link
!> data. It delegates boundary parsing to [[JEOCBC]] and link geometry to
!> [[OCPLF]].
!>
!> The routine follows the OC input record order used in the SHETRAN Data
!> Input Manual:
!>
!> | Records | Action |
!> |:--------|:-------|
!> | `OC1` | Read `NT`, roughness-category count `NCATR`, print/output control `KONT`, and `BIOWAT`. Odd `KONT` values enable verbose input echoing. |
!> | `OC2` | Skip the obsolete OC timestep pairs; the current code reads and discards this section. |
!> | `OC3` | Read `SMIN`, default roughness `CDRS`, output interval `TDC:TFC`, and `DET`. If `KONT < 2`, output is disabled by setting `TDC > TFC`. |
!> | `OC4` | If `CDRS=0` and `NCATR>0`, read the category roughness values `CATR`. |
!> | `OC5` | Read initial overland water depth when `BIOWAT` is true; otherwise initialise it to zero. `HRF` is set to `ZGRUND + depth` for land elements. |
!> | `OC14`/`OC17` | Populate `STRXX` and `STRYY` from `CDRS`, direct arrays, or category maps. |
!> | Boundary records | Call [[JEOCBC]] for OC boundary metadata, then [[OCPLF]] for channel-link geometry and link-boundary details. |
!>
!> `OCD` is rewound, not closed, after the read. Any boundary or channel-link
!> input errors collected during parsing are promoted to fatal error 1049.
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NELEE >= max(total_no_elements, NOCTAB*NOCTAB)` | Element and temporary OC tables fit the compiled extent. |
!> | `total_no_elements > total_no_links` and `total_no_links >= 0` | Land elements and optional links are consistently numbered. |
!> | `NOCTAB >= 1` and `NLFEE >= total_no_links` | Boundary and link tables cover active data. |
!> | `OCD` open for formatted input | OC input records can be read. |
!> | `PRI` open for formatted output | Input echo and diagnostics can be written. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-01-20 | RAH | 4.2 | Created this routine, implementing the previously-missing `NCATR>0` option. |
!> | 1998-02-26 | RAH | 4.2 | Moved `TDC`/`TFC` to the argument list, overwriting `TDC` when `KONT<2`. |
!> | 2026-04-02 | SvB | - | Modernised the routine's loops and error handling with Gemini assistance (replaced `GOTO`-based control flow with block `IF`s and array slicing). |
!> @endhistory
   SUBROUTINE OCREAD(KONT, TDC, TFC, CATR, DDUM2)

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(OUT)          :: KONT   !! Print/output control; odd values enable verbose echoing.
      DOUBLE PRECISION, INTENT(OUT) :: TDC    !! First time for detailed OC diagnostic output.
      DOUBLE PRECISION, INTENT(OUT) :: TFC    !! Last time for detailed OC diagnostic output.
      DOUBLE PRECISION, INTENT(OUT) :: CATR(NOCTAB) !! Roughness coefficient by category, when `NCATR>0`.
      DOUBLE PRECISION, INTENT(OUT) :: DDUM2(NOCTAB, NOCTAB) !! Discarded scratch passed through to [[occhk2]].

      ! Locals
      INTEGER          :: I, IBC, ICAT, ielt, IXER, KKON, TYPEE
      INTEGER          :: NCATR, NLAND, NOCBC, NT
      DOUBLE PRECISION :: DET, SMIN, CDRS
      LOGICAL          :: BIOWAT, BOUT
      CHARACTER(81)    :: MSG

      INTEGER, PARAMETER :: NC(11) = [0, 0, 0, 0, 5, 0, 4, 4, 0, 0, 5]
      CHARACTER(11), PARAMETER :: CTYPE(11) = ['impermeable', '  grid-grid', '       head', ' flux      ', &
                            ' polynomial', ' river_link', '       weir', ' river+weir', '       head', '       flux', ' polynomial']

      !----------------------------------------------------------------------*
      !              Initialization
      !
      IXER = 0
      NLAND = total_no_elements - total_no_links
      NGDBGN = total_no_links + 1

      !              Integer & logical variables
      ! :OC1
      READ (OCD, *)
      READ (OCD, *) NT, NCATR, KONT, BIOWAT

      KKON = MOD(KONT, 2)
      BOUT = (KKON == 1)

      IF (BOUT) WRITE (PPPRI, 9080) ' ', NCATR

      !              OC time-step data
      ! :OC2
      READ (OCD, *)
      READ (OCD, *)

      !              Default roughness parameters & floating-point variables
      ! :OC3
      READ (OCD, *)
      READ (OCD, *) SMIN, CDRS, TDC, TFC, DET

      IF (KONT < 2) TDC = TFC + one

      ! :OC4
      IF (ISZERO(CDRS)) THEN
         IF (NCATR > NOCTAB .OR. NCATR < 0) THEN
            WRITE (MSG, '("Number of roughness categories NCATR =",I4,2X, &
&                         "lies outside range 0:NOCTAB = 0 :",I4)') NCATR, NOCTAB
            CALL ERROR(FFFATAL, 1047, PPPRI, 0, 0, MSG)
         END IF

         IF (NCATR > 0) THEN
            ! PERF FIX: Implied DO loop instead of array slice
            READ (OCD, *) (CATR(I), I=1, NCATR)
            IF (BOUT) THEN
               WRITE (PPPRI, 9084) (CATR(I), I=1, NCATR)
               WRITE (PPPRI, *)
            END IF
         END IF
      ELSE IF (BOUT) THEN
         WRITE (PPPRI, 9082) CDRS
      END IF

      !              INITIAL OVERLAND FLOW ELEVATIONS
      ! :OC5
      IF (BIOWAT) THEN
         CALL AREADR(DUMMY, KKON, OCD, PPPRI)
      ELSE
         ! PERF FIX: Explicit DO loop instead of array slice assignment
         DO ielt = NGDBGN, total_no_elements
            DUMMY(ielt) = ZERO
         END DO
         IF (BOUT) WRITE (PPPRI, 9085) 'zero'
      END IF

      elevation_loop: DO ielt = NGDBGN, total_no_elements
         CALL SETHRF(ielt, ZGRUND(ielt) + DUMMY(ielt))
      END DO elevation_loop

      !              ROUGHNESS PARAMETERS FOR OVERLAND FLOW
      ! :OC14
      ! :OC17
      IF (NOTZERO(CDRS)) THEN
         ! PERF FIX: Explicit DO loops instead of array slice assignment
         DO ielt = NGDBGN, total_no_elements
            STRXX(ielt) = CDRS
            STRYY(ielt) = CDRS
         END DO
      ELSE IF (NCATR == 0) THEN
         CALL AREADR(STRXX, KKON, OCD, PPPRI)
         CALL AREADR(STRYY, KKON, OCD, PPPRI)
      ELSE
         ! Pass base memory address IDUM
         CALL AREADI(IDUM, KKON, OCD, PPPRI, NCATR)

         roughness_x_loop: DO ielt = NGDBGN, total_no_elements
            ICAT = MAX(1, MIN(IDUM(ielt), NCATR))
            STRXX(ielt) = CATR(ICAT)
         END DO roughness_x_loop

         CALL AREADI(IDUM, KKON, OCD, PPPRI, NCATR)

         roughness_y_loop: DO ielt = NGDBGN, total_no_elements
            ICAT = MAX(1, MIN(IDUM(ielt), NCATR))
            STRYY(ielt) = CATR(ICAT)
         END DO roughness_y_loop
      END IF

      !              BOUNDARY CONDITIONS
      CALL JEOCBC(IXER, NOCBC)

      !              PARAMETERS OF RIVER LINKS
      IF (total_no_links > 0 .AND. IXER == 0) THEN
         ! PERF FIX: Pass base memory address NOCBCD(1, 2) instead of 2D slice
         CALL OCPLF(BOUT, IXER, NOCBCD(1, 2), IDUM, DDUM2)
      END IF

      !              FINISH
      REWIND (OCD)

      IF (IXER /= 0) THEN
         WRITE (MSG, 9412) IXER
         CALL ERROR(FFFATAL, 1049, PPPRI, 0, 0, MSG)
      ELSE IF (BOUT) THEN
         WRITE (PPPRI, 9500) 'no-flow'
         IF (NOCBC > 0) WRITE (PPPRI, 9600) 'Index', 'Element', 'Face', &
            'Type', 'Category', 'Coefficients'

         print_bc_loop: DO IBC = 1, NOCBC
            TYPEE = NOCBCD(IBC, 3)

            ! PERF FIX: Explicit indexing and Implied DO loop instead of slices
            WRITE (PPPRI, 9610) IBC, NOCBCD(IBC, 1), NOCBCD(IBC, 2), CTYPE(TYPEE), &
               NOCBCD(IBC, 4), (COCBCD(I, IBC), I=1, NC(TYPEE))
         END DO print_bc_loop

         WRITE (PPPRI, 9080) ' END OF '
      END IF

      RETURN

      ! FORMAT STATEMENTS
9080  FORMAT(///'---- OC MODULE ', A, 'INPUT DATA PROCESSING ----'///: &
              5X, 'NUMBER OF DIFFERENT OVERLAND FLOW ROUGHNESS', &
              ' CATEGORIES   NCATR = ', I4)

9082  FORMAT(/5X, 'DEFAULT VALUE OF OVERLAND FLOW ROUGHNESS ', &
              'COEFFICIENT     CDRS = ', F8.2)

9084  FORMAT(/4X, ' ROUGHNESS COEFFICIENTS  CATR  ATTACHED TO', &
              ' EACH OF THE NCATR CATEGORIES'/(10F10.2))

9085  FORMAT(/5X, 'Initial overland water depth is ', A)

9412  FORMAT(I5, ' ERROR(S) FOUND DURING OC INPUT DATA PROCESSING')

9500  FORMAT(/5X, 'Default OC B.C. is ', A, ' at catchment boundaries ', &
              'and at channel/bank dead-ends')

9600  FORMAT(/5X, 'OC Boundary Conditions:'//5X, 3A8, A12, A10, A14)

9610  FORMAT(5X, 3I8, A12, I10, 1P, 5G14.6)
   END SUBROUTINE OCREAD

!> @brief Advances the overland/channel flow solution by one OC timestep.
!>
!> `OCSIM` reads current boundary values, calls [[OCQDQ]] for nonlinear flow
!> and derivative terms, assembles the row-wise implicit matrix with
!> [[OCABC]], solves the block tridiagonal system by forward row elimination
!> and backward substitution, updates water levels and inter-element flows,
!> applies [[OCFIX]] to remove spurious negative internal flows, computes
!> channel wetted area, and optionally prints OC diagnostics.
!>
!> The routine uses frame topology and geometry (`ICMREF`, `ICMRF2`,
!> `NOCBCC`, `NOCBCD`, `cellarea`, `CLENTH`, `DHF`, `ZGRUND`, `CWIDTH`,
!> `ZBFULL`), forcing terms (`PNETTO`, `ESWA`, `QH`, `QOCF`, `HOCNOW`), OC
!> row indices (`NROWF`, `NROWL`, `NROWST`, `NROWEL`, `NELIND`),
!> cross-section and roughness tables (`NXSECT`, `XINH`, `XINW`, `XAREA`,
!> `XSTAB`, `STRXX`, `STRYY`), and timing controls (`OCNOW`, `OCNEXT`,
!> `TDC`, `TFC`). It updates `HRF` and writes `QSA`, `QOC`, `DQ0ST`,
!> `DQIST`, `DQIST2`, and `ARXL`. The per-link bankfull area `XAFULL` that
!> [[OCQDQ]] reads is a static property of the cross-section tables and is
!> built once by [[ocxs]] during initialisation, not here.
!>
!> The OC timestep is converted to seconds as
!>
!> \[
!> DTOC = 3600\,OCNEXT.
!> \]
!>
!> After [[OCQDQ]] has evaluated current flows and derivatives, [[OCABC]]
!> assembles one block row for each y-row. For row \(r\), with water-level
!> correction vector \(d_r\), the assembled equation has the
!> block-tridiagonal form
!>
!> \[
!> C_r d_{r-1} + B_r d_r + A_r d_{r+1} = F_r,
!> \]
!>
!> where `CC`, `BB`, and `AA` contain the previous-row, current-row, and
!> next-row coefficients, and `FF` contains \(F_r\).
!>
!> The forward sweep stores each eliminated row as
!>
!> \[
!> d_{r-1} = E_r d_r + G_r.
!> \]
!>
!> For the first active row,
!>
!> \[
!> M_r=B_r,\qquad v_r=F_r.
!> \]
!>
!> For later rows, the previous-row relation is substituted:
!>
!> \[
!> M_r = B_r + C_rE_r,\qquad
!> v_r = F_r - C_rG_r.
!> \]
!>
!> The row matrix is inverted and the relation for the next row is stored as
!>
!> \[
!> E_{r+1} = -M_r^{-1}A_r,\qquad
!> G_{r+1} = M_r^{-1}v_r.
!> \]
!>
!> Back substitution starts with the last row,
!>
!> \[
!> d_{NROWL}=G_{NROWL+1},
!> \]
!>
!> and proceeds upward with
!>
!> \[
!> d_r = E_{r+1}d_{r+1}+G_{r+1}.
!> \]
!>
!> The solved correction for each element is applied directly to water
!> level,
!>
!> \[
!> HRF_e^{n+1}=HRF_e^n+d_e.
!> \]
!>
!> Face flows are advanced with the same first-order linearisation used in
!> the matrix assembly:
!>
!> \[
!> Q_{e,f}^{n+1}=Q_{e,f}^n + DQ0ST_{e,f}d_e
!> + \sum_j DQIST_{e,f,j}d_j,
!> \]
!>
!> where a single neighbour uses `DQIST` and a multi-link junction expands
!> the neighbour sum through `ICMRF2` and `DQIST2`. [[OCFIX]] is then called
!> to remove spurious negative internal flows and adjust the corresponding
!> water levels.
!>
!> `QOC` is copied from the internal face-flow array and converted from the
!> OC face convention to the model x/y convention by changing the sign on
!> faces 1 and 2. For each channel link, wetted area is interpolated from
!> the cross-section table. If \(H=HRF-ZGRUND\) falls between tabulated
!> depths \(H_m\) and \(H_i\),
!>
!> \[
!> \Delta H = H-H_m,\qquad
!> \Delta W = (W_i-W_m)\frac{\Delta H}{H_i-H_m},
!> \]
!>
!> \[
!> ARXL = XAREA_m + \left(W_m+\frac{1}{2}\Delta W\right)\Delta H.
!> \]
!>
!> Above the last table level, the link is extended with rectangular
!> bankfull width:
!>
!> \[
!> ARXL = XAREA_N + (HRF-ZBFULL)\,CWIDTH.
!> \]
!>
!> If `QMAX > 0`, all channel-link face flows are checked against this
!> maximum and a fatal diagnostic is issued when it is exceeded.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 3.4-4.2 | Developed the row-wise implicit solve, [[OCFIX]] flow-correction split, and current `OCABC` argument list. |
!> | 2009-01 | JE | - | Restructured the row loop for automatic differentiation. |
!> @endhistory
   SUBROUTINE OCSIM

      IMPLICIT NONE

      INTEGER :: I, IELs, IND, IROW, IBC, IBR, ICOD, IFACE, IHB, IM, IRSV
      INTEGER :: J, JEL, JND, JROW, K0, LINK, N, NCR, NPR, NSV, face
      INTEGER :: kk, ll, vv

      DOUBLE PRECISION :: DDI, DH, DQ, DW, H, HI, HM, OCTIME, WI, WM, Z

      LOGICAL :: found_level, channel_blowup
      CHARACTER(36) :: MSG

      !----------------------------------------------------------------------*
      !
      ! ----- Initialize
      ijedum = 0
      ijedum2 = 0

      AA = 0.0D0; DD = 0.0D0; FF = 0.0D0; BB = 0.0D0; GG = 0.0D0
      CC = 0.0D0; EE = 0.0D0; TM1 = 0.0D0; TM2 = 0.0D0; TV1 = 0.0D0; TV2 = 0.0D0

      inhrf = 0.0D0; GGGETHRF = 0.0D0; inqsa = 0.0D0; GGGETQSA = 0.0D0

      ! ----- Timestep setup
      DTOC = OCNEXT*3600.0D0

      ! ----- GET PRESCRIBED BOUNDARY VALUES HOCNOW & QOCF
      CALL OCEXT

      ! ----- CALCULATE FLOWS QSA & DERIVATIVES DQ0ST,DQIST,DQIST2
      CALL OCQDQ()

      ! ----- LOOP OVER ROWS, CALCULATING EE & GG
      NCR = 0

      row_loop: DO IROW = NROWF, NROWL
         IRSV = IROW + 1
         !
         ! NCR : NUMBER OF ELEMENTS IN THE CURRENT ROW
         ! NPR : NUMBER OF ELEMENTS IN THE PREVIOUS ROW
         ! NSV : NUMBER OF ELEMENTS IN THE NEXT (SUIVANT) ROW
         !
         NPR = NCR
         K0 = NROWST(IROW) - 1
         NCR = NROWST(IRSV) - 1 - K0

         IF (NCR == 0) CYCLE row_loop

         NSV = NROWST(MIN(IRSV, NROWL) + 1) - NROWST(IRSV)

         ! CALCULATE MATRICES AA, BB, CC, FF
         DO IND = 1, NCR
            iels = NROWEL(IND + K0)
            LINK = MAX(1, MIN(iels, total_no_links))
            IBC = NOCBCC(iels)

            IF (IBC > 0) THEN
               IHB = NOCBCD(IBC, 4)
               IBC = NOCBCD(IBC, 3)
            ELSE
               IHB = 1
            END IF

            CALL OCABC(IND, IROW, iels, NSV, NCR, NPR, IBC, NXSECT(LINK), cellarea(iels), &
                       ZGRUND(iels), CLENTH(LINK), ZBFULL(LINK), GETHRF(iels), &
                       PNETTO(iels), QH(iels), ESWA(iels), HOCNOW(IHB), AA(:, IND), &
                       BB(1:ncr, IND), CC(:, IND), FF(IND))
         END DO

         ! CALCULATE MATRIX TM2 (inverse of CC.EE+BB) AND VECTOR TV2 (FF-CC.GG)
         IF (IROW == NROWF) THEN
            DO IND = 1, NCR
               TM2(1:ncr, IND) = BB(1:ncr, IND)
            END DO
            TV2(1:ncr) = FF(1:ncr)
         ELSE
            tm1(1:ncr, 1:ncr) = JEMATMUL_MM(cc(1:npr, 1:ncr), ee(1:ncr, 1:npr, irow), ncr, npr, ncr)
            tm2(1:ncr, 1:ncr) = bb(1:ncr, 1:ncr) + tm1(1:ncr, 1:ncr)
            tv1(1:ncr) = JEMATMUL_VM(cc(1:npr, 1:ncr), gg(1:npr, irow), ncr, npr)
            TV2(1:ncr) = FF(1:ncr) - TV1(1:ncr)
         END IF

         CALL INVERTMAT(TM2(1:ncr, 1:ncr), NCR, ICOD)

         ! Catch singular matrix inversion failure
         IF (ICOD == 1) THEN
            WRITE (MSG, '(A,I4)') 'Singular matrix at row', IROW
            CALL ERROR(FFFATAL, 1018, PPPRI, NROWEL(NROWST(IROW)), 0, MSG)
            RETURN
         END IF

         ! CALCULATE MATRIX EE(IROW+1)
         IF (IROW /= NROWL) THEN
            ee(1:nsv, 1:ncr, irsv) = JEMATMUL_MM(tm2(1:ncr, 1:ncr), aa(1:nsv, 1:ncr), ncr, ncr, nsv)
            ee(1:nsv, 1:ncr, irsv) = -ee(1:nsv, 1:ncr, irsv)
         END IF

         ! CALCULATE VECTOR GG(IROW+1)
         gg(1:ncr, irsv) = JEMATMUL_VM(tm2(1:ncr, 1:ncr), tv2(1:ncr), ncr, ncr)

      END DO row_loop

      ! ----- DOWNWARDS SWEEP, CALCULATION OF DD
      !
      ! * last row first (use NCR,IRSV from loop above)
      IROW = NROWL
      DD(1:ncr, IROW) = GG(1:ncr, IRSV)

      ! * loop over remaining rows
      DO IROW = NROWL - 1, NROWF, -1
         IRSV = IROW + 1
         NSV = NCR
         NCR = NROWST(IRSV) - NROWST(IROW)

         tv1(1:ncr) = JEMATMUL_VM(ee(1:nsv, 1:ncr, irsv), dd(1:nsv, irsv), ncr, nsv)
         dd(1:ncr, irow) = tv1(1:ncr) + gg(1:ncr, irsv)
      END DO

      ! ----- ADVANCE WATER LEVELS AND FLOWS TO TIME LEVEL N+1,
      !       USING FIRST ORDER DERIVATIVES OF FLOWS AT TIME LEVEL N
      DO iels = 1, total_no_elements
         IND = NELIND(iels)
         IROW = ICMREF(iels, 3)
         DDI = DD(IND, IROW)
         CALL SETHRF(iels, GETHRF(iels) + DDI)

         DO IFACE = 1, 4
            DQ = DQ0ST(iels, IFACE)*DDI
            JEL = ICMREF(iels, IFACE + 4)

            IF (JEL > 0) THEN
               JND = NELIND(JEL)
               JROW = ICMREF(JEL, 3)
               DQ = DQIST(iels, IFACE)*DD(JND, JROW) + DQ

            ELSE IF (JEL < 0) THEN
               IBR = -JEL
               DO J = 1, 3
                  JEL = ICMRF2(IBR, J)
                  IF (JEL > 0) THEN
                     JND = NELIND(JEL)
                     JROW = ICMREF(JEL, 3)
                     DQ = DQIST2(IBR, J)*DD(JND, JROW) + DQ
                  END IF
               END DO
            END IF

            CALL SETQSA(iels, IFACE, GETQSA(iels, IFACE) + DQ)
         END DO
      END DO

      ! CHECK FOR SPURIOUS NEGATIVE FLOWS, AND RECALCULATE WATER LEVELS
      ! IF REQUIRED.  NB. DOES NOT CHECK BOUNDARY FLOWS
      vv = 5
      DO LL = 2, 3
         DO kk = 1, 4
            ijedum(:, kk, LL) = icmref(:, vv)
            vv = vv + 1
         END DO
      END DO

      vv = 1
      DO LL = 1, 2
         DO kk = 1, 3
            ijedum2(:, kk, LL) = icmrf2(:, vv)
            vv = vv + 1
         END DO
      END DO

      ! untidy mess for debugging of tangent
      DO vv = 1, total_no_elements
         inhrf(vv) = GETHRF(vv)
         DO face = 1, 4
            inqsa(vv, face) = GETQSA(vv, face)
         END DO
      END DO

      CALL OCFIX(ijedum, ijedum2, total_no_elements, dtoc, inhrf, GGGETHRF, inqsa, GGGETQSA)

      DO vv = 1, total_no_elements
         CALL SETHRF(vv, GGGETHRF(vv))
         DO face = 1, 4
            CALL SETQSA(vv, face, GGGETQSA(vv, face))
         END DO
      END DO

      ! SET FLOWS QOC (POSITIVE X,Y) FOR USE BY OTHER COMPONENTS
      QOC(1:total_no_elements, :) = GETQSA_ALL(total_no_elements)
      qoc(1:total_no_elements, 1:2) = -qoc(1:total_no_elements, 1:2)

      ! ----- CALCULATE CROSS-SECTIONAL AREA OF CHANNEL WATER
      link_loop: DO iels = 1, total_no_links
         Z = GETHRF(iels)
         H = Z - ZGRUND(iels)
         N = NXSECT(iels)
         found_level = .FALSE.

         sect_loop: DO I = 2, N
            HI = XINH(iels, I)
            IF (H < HI) THEN
               IM = I - 1
               HM = XINH(iels, IM)
               WM = XINW(iels, IM)
               WI = XINW(iels, I)
               DH = H - HM
               DW = (WI - WM)*(DH/(HI - HM))
               ARXL(iels) = XAREA(iels, IM) + (WM + 0.5D0*DW)*DH
               found_level = .TRUE.
               EXIT sect_loop
            END IF
         END DO sect_loop

         IF (.NOT. found_level) THEN
            ARXL(iels) = XAREA(iels, N) + (Z - ZBFULL(iels))*CWIDTH(iels)
         END IF
      END DO link_loop

      ! ----- Print results
      OCTIME = OCNOW + OCNEXT
      IF ((OCTIME >= TDC) .AND. (OCTIME <= TFC)) CALL OCPRI(OCTIME, ARXL, QOC)

      ! ----- CHECK FOR CHANNEL BLOW-UP
      channel_blowup = .FALSE.
      IF (GTZERO(QMAX)) THEN
         blowup_loop: DO iels = 1, total_no_links
            DO IFACE = 1, 4
               IF (ABS(QOC(iels, IFACE)) > QMAX) THEN
                  channel_blowup = .TRUE.
                  EXIT blowup_loop
               END IF
            END DO
         END DO blowup_loop
      END IF

      IF (channel_blowup) THEN
         MSG = 'CHANNEL FLOWS EXCEED MAXIMUM ALLOWED'
         CALL ERROR(FFFATAL, 1029, PPPRI, iels, 0, MSG)
      END IF

   END SUBROUTINE OCSIM

!> @brief Builds channel cross-section area and conveyance lookup tables.
!>
!> `OCXS` integrates tabulated width-depth pairs to water area, derives an
!> effective bed elevation for full-bank storage, and fills `XSTAB` with
!> uniformly spaced depth, conveyance, and conveyance-derivative values used
!> by the OC flow calculation.
!>
!> The SHETRAN User Guide and Data Input Manual defines the channel
!> cross-section data in records `OC30`-`OC34`: each cross-section category
!> is supplied as width/depth pairs (`XDEFW`, `XDEFH`), the first depth must
!> be zero, and the final depth defines the bankfull depth. The manual also
!> states that channel flow uses the user-supplied cross-section, while
!> subsurface-flow exchange uses an effective rectangular channel with the
!> same cross-sectional area.
!>
!> In the code these manual fields are stored per link as `XINW(link,j)` and
!> `XINH(link,j)`, with `NXSECT(link)` width/depth pairs and roughness
!> `STRXX(link)`. Entry requirements retained from the legacy routine are:
!> at least one active link, `NXSCEE >= 2`, positive channel widths
!> `CWIDTH(1:total_no_links)`, `NXSECT` values within the allocated
!> `XINH`/`XINW`/`XAREA` table sizes, a positive final tabulated depth, and
!> strictly increasing tabulated depths within each link.
!>
!> Input width-depth pairs are integrated by the trapezoidal rule. For
!> tabulated level \(j\),
!>
!> \[
!> XAREA_j = XAREA_{j-1}
!> + \frac{1}{2}\left(XINW_j+XINW_{j-1}\right)
!>   \left(XINH_j-XINH_{j-1}\right),
!> \]
!>
!> with `XAREA(:,1)=0`. The manual's effective rectangular-channel statement
!> is implemented by shifting the effective bed elevation so that a
!> rectangle of width `CWIDTH` has the same bankfull area as the tabulated
!> cross-section:
!>
!> \[
!> ZBEFF = ZBFULL - XAREA_N/CWIDTH.
!> \]
!>
!> The same bankfull area is retained per link as
!> `XAFULL(link) = XAREA(link,NXSECT(link))` for [[ocqdqmod:ocqdq]]. It
!> depends only on the cross-section tables, so it is built here once rather
!> than on the first [[ocsim]] call.
!>
!> The lookup table `XSTAB` supports the OC flow calculation without
!> repeatedly integrating the irregular cross-section. It has rows:
!>
!> | `XSTAB` row | Meaning |
!> |:------------|:--------|
!> | 1 | Uniformly spaced water depth. |
!> | 2 | Conveyance at that depth. |
!> | 3 | Piecewise-linear derivative of conveyance with respect to depth. |
!>
!> For a table with `NXSCEE` rows, the uniform depth increment is
!>
!> \[
!> \Delta h = XINH_N/(NXSCEE-1),\qquad h_j=(j-1)\Delta h.
!> \]
!>
!> For each lookup depth \(h_j\), the enclosing manual input interval
!> \(H_i \le h_j \le H_{i+1}\) is found. Width is treated as linearly
!> varying between the two tabulated width/depth points:
!>
!> \[
!> \alpha=\frac{h_j-H_i}{H_{i+1}-H_i},
!> \]
!>
!> and the area increment above \(H_i\) is evaluated as a trapezoid:
!>
!> \[
!> A_j = XAREA_i
!> + \frac{1}{2}\left((2-\alpha)W_i+\alpha W_{i+1}\right)
!>   (h_j-H_i).
!> \]
!>
!> `CONVEYAN` converts \(A_j\), depth, and roughness `STRXX` into
!> conveyance. For `OCXS` it is called with `ty=0`, so the main branch used
!> away from near-zero depth is the Gauckler-Manning-Strickler-style
!> relation implemented in [[ocmod2:conveyan]]:
!>
!> \[
!> C_j = STRXX\,A_j\,h_j^{2/3}.
!> \]
!>
!> For \(10^{-9} \le h_j < 10^{-3}\) m, the code uses the smoothed
!> polynomial branch in `CONVEYAN` for automatic-differentiation stability:
!>
!> \[
!> C_j = STRXX\,A_j\,\frac{10}{3}\,h_j(4-1000h_j),
!> \]
!>
!> and for smaller depths it returns zero conveyance. The stored derivative
!> in `XSTAB` is not the derivative returned by `CONVEYAN`; `OCXS` stores
!> the finite-difference slope for interval `j-1`:
!>
!> \[
!> XSTAB_{3,j-1} = \frac{C_j-C_{j-1}}{\Delta h},
!> \]
!>
!> so `XSTAB(2,j) + XSTAB(3,j)*(h-XSTAB(1,j))` is continuous and
!> piecewise linear in water depth. As in the legacy routine, conveyance and
!> derivative entries for the final lookup row are not defined.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-03 | RAH | 4.2 | Created this routine, taking it from part of [[OCPLF]]. |
!> | 1998-03-17 | RAH | 4.2 | Fixed the `XAJ` inaccuracy so the stored conveyance derivative is continuous. |
!> | 1998-04-24 | RAH | 4.2 | Merged the legacy `XSECTH`/`XCONV`/`XDERIV` arrays into `XSTAB`. |
!> | 2026-08-18 | SvB | 4.6.5 | Moved the `XAFULL` setup here from the first-call branch of [[ocsim]], reusing this routine's existing per-link loop. |
!> @endhistory
   SUBROUTINE OCXS()

      IMPLICIT NONE

      INTEGER         :: I, IELr, J, N
      DOUBLE PRECISION :: ALPHA, DH, HI, HIP1, HJ, STEPH, STR, W2, XAJ, XCJ, XCJM1, adumy

      !----------------------------------------------------------------------*

      link_loop: DO ielr = 1, total_no_links
         !
         ! LOCAL VARIABLES
         !
         N = NXSECT(ielr)
         STR = STRXX(ielr)

         !
         ! SET UP CROSS-SECTIONAL AREAS FOR EACH OF THE INPUT LEVELS
         !
         XAREA(ielr, 1) = zero

         area_loop: DO J = 2, N
            W2 = XINW(ielr, J) + XINW(ielr, J - 1)
            DH = XINH(ielr, J) - XINH(ielr, J - 1)
            XAREA(ielr, J) = XAREA(ielr, J - 1) + W2*DH*half
         END DO area_loop

         !
         ! EFFECTIVE BED ELEVATION
         !
         ZBEFF(ielr) = ZBFULL(ielr) - XAREA(ielr, N)/CWIDTH(ielr)

         !
         ! FULL-FLOW AREA FOR OCQDQ: the top row of this link's area table
         !
         XAFULL(ielr) = XAREA(ielr, N)

         !
         ! SET UP FULL CROSS-SECTION TABLES OF HEIGHT, CONVEYANCE & DERIVATIVE
         !
         ! NOTE: The formulation is such that
         !             XSTAB(2,j,ielr) + XSTAB(3,j,ielr)*( h - XSTAB(1,j,ielr) )
         !       is a continuous (piecewise linear) function of h
         !
         I = 1
         HI = XINH(ielr, I)
         STEPH = XINH(ielr, N)/(NXSCEE - 1.0d0)
         XCJ = zero
         XSTAB(1, 1, ielr) = zero

         table_loop: DO J = 2, NXSCEE
            XCJM1 = XCJ
            HJ = STEPH*(J - 1)

            ! Advance index I until we bracket the target height HJ
            search_loop: DO
               HIP1 = XINH(ielr, I + 1)
               IF (I >= N - 1 .OR. HIP1 >= HJ) EXIT search_loop
               I = I + 1
               HI = HIP1
            END DO search_loop

            DH = HJ - HI
            ALPHA = DH/(HIP1 - HI)
            W2 = (2.0d0 - ALPHA)*XINW(ielr, I) + ALPHA*XINW(ielr, I + 1)
            XAJ = XAREA(ielr, I) + W2*DH*half

            ! XCJ = STR * XAJ * HJ**F23
            CALL CONVEYAN(str, hj, xcj, adumy, 0, xaj)

            XSTAB(1, J, ielr) = HJ
            XSTAB(2, J - 1, ielr) = XCJM1
            XSTAB(3, J - 1, ielr) = (XCJ - XCJM1)/STEPH
         END DO table_loop

      END DO link_loop

   END SUBROUTINE OCXS

!> @brief Returns the channel link number at a grid coordinate and orientation.
!>
!> `LINKNO` searches the link reference table for a north-south or east-west
!> link whose stored grid coordinate matches the requested `(I,J)` location.
!> The orientation argument is compared directly with `LINKNS`; no geometric
!> inference is made here. If there are no links, or no matching link is
!> found, the function returns zero.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04 | SvB | - | Marked the function `PURE` and replaced the legacy `iscycle`-flag loop with a direct `EXIT`, without changing its search order or result. |
!> @endhistory
   PURE INTEGER FUNCTION LINKNO(I, J, NSOUTH)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: I      !! Grid x-coordinate.
      INTEGER, INTENT(IN) :: J      !! Grid y-coordinate.
      LOGICAL, INTENT(IN) :: NSOUTH !! True to match a north-south link; false for east-west.

      ! Locals
      INTEGER :: L

      !----------------------------------------------------------------------*

      LINKNO = 0

      IF (total_no_links == 0) RETURN

      ! High-Performance Fix: Replaced 'iscycle' AD-hack with a direct EXIT
      ! to immediately terminate the loop once the correct link is found.
      search_loop: DO L = 1, total_no_links

         ! Integer comparison first for fast short-circuiting
         IF (ICMREF(L, 2) == I .AND. ICMREF(L, 3) == J) THEN

            ! Logical equivalence check
            IF (NSOUTH .EQV. LINKNS(L)) THEN
               LINKNO = L
               EXIT search_loop
            END IF

         END IF

      END DO search_loop

   END FUNCTION LINKNO

END MODULE OCmod
