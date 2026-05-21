!> summary: Overland and channel flow routing.
!>
!> This module implements the SHETRAN overland/channel flow (OC) component. It
!> reads OC input, builds channel link geometry and boundary-condition tables,
!> computes channel cross-section lookup tables, sets up row-wise indexing for
!> the implicit solver, and advances free-surface elevations and inter-element
!> flows during the simulation.
!>
!> The timestep solve is a row-swept implicit finite-difference system. Each
!> active row is assembled as a block tridiagonal coupling to the previous,
!> current, and next rows, inverted row by row, and then back-substituted in a
!> downward sweep. Channel conveyance is derived from the OC input-file
!> width/depth cross-section tables and Strickler roughness coefficients through
!> [[ocmod2:conveyan]]. Boundary categories, channel geometry, and roughness
!> controls correspond to the manual's Overland/Channel Module section.
!>
!> History:
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989-1998 | GP/RAH | 2.0-4.2 | Developed the implicit OC scheme, banks, hot-start state migration, boundary-condition arrays, row indexing, and merged channel cross-section lookup table `XSTAB`. |
!> | 2008-12 | JE | 4.3.5F90 | Created as part of the Fortran 90 conversion, replacing part of the legacy OC `.F` files. |
!> | 2026-03 | SB | 4.6 | Moved large OC solver, water-surface, discharge, and index work arrays to allocatable storage. |
MODULE OCmod
USE SGLOBAL
USE AL_C ,     ONLY : IDUM, NBFACE, CWIDTH, ZBFULL, &
                     DUMMY, ZBEFF, ICMBK, BEXBK, QBKB, QBKF, ICMRF2, &
                     TIH, DHF, CLENTH, CLENTH, PNETTO, QH, QOC, LINKNS, ARXL
USE AL_D ,     ONLY : DQ0ST, DQIST, DQIST2, OCNOW, OCNEXT, OCD, ESWA, QMAX, NOCBCC, &
                     NOCBCD, LCODEX, LCODEY, NOCTAB, OHB, OFB
USE AL_G ,     ONLY : NGDBGN, NX, NY, ICMREF, ICMXY
USE UTILSMOD , ONLY : HINPUT, FINPUT, AREADR, AREADI, JEMATMUL_VM, JEMATMUL_MM, INVERTMAT
USE mod_load_filedata ,    ONLY : ALCHK, ALCHKI, ALINIT
USE OCmod2 ,   ONLY : GETHRF, GETQSA, GETQSA_ALL, SETHRF, SETQSA, CONVEYAN, OCFIX, XSTAB, &
                      HRFZZ, qsazz, INITIALISE_OCMOD  !these needed only for ad
USE OCQDQMOD,  ONLY : OCQDQ, STRXX, STRYY, HOCNOW, QOCF, XAFULL, COCBCD !, &  !REST NNEDED ONLY FOR AD
 !                     firstocqdq
! Legacy SPEC.OC component variables retained as module state.
! Requires NXSCEE >= 2 for channel cross-section lookup tables.
IMPLICIT NONE
INTEGER            :: NELIND (NELEE)
INTEGER            :: NROWF, NROWL, NOCHB, NOCFB
INTEGER            :: NROWEL (NELEE), NROWST (NYEE+1), NXSECT (NLFEE)
DOUBLEPRECISION    :: HOCLST, HOCNXT, QFLAST, QFNEXT, TDC, TFC
DOUBLEPRECISION    :: HOCPRV (NOCTAB), QOCFIN (NOCTAB), HOCNXV (NOCTAB)
DOUBLEPRECISION    :: XINH (NLFEE, NOCTAB)
DOUBLEPRECISION    :: XINW (NLFEE, NOCTAB)
DOUBLEPRECISION    :: XAREA (NLFEE, NOCTAB)
DOUBLEPRECISION     :: dtoc

PRIVATE

 PUBLIC :: OCINI, OCSIM, OCLTL, LINKNO, & !REST ARE PUBLIC FOR AD ONLY
          qfnext, hoclst, hocprv, qocfin, hocnxt, hocnxv

CONTAINS

!> Controls OC component initialisation.
!>
!> `OCINI` checks static dimensions and topology, reads the OC input file,
!> validates roughness and cross-section data, opens boundary files, initialises
!> OC state held in [[OCmod2]], builds channel cross-section tables through
!> [[OCXS]], and prepares row indices through [[OCIND]].
!>
!> Entry requirements are the OC/frame array limits being positive
!> (`NELEE`, `NLFEE`, `NXEE`, `NY`, `NOCTAB`), at least two internal
!> cross-section table rows (`NXSCEE >= 2`), and an element index range with
!> `total_no_elements >= NGDBGN`.
!>
!> The routine uses frame geometry and topology from `ICMREF`, `ICMBK`, `ICMXY`,
!> `LCODEX`, `LCODEY`, `NBFACE`, and `LINKNS`, plus OC input/output units
!> `OCD`, `OHB`, `OFB`, and `PRI`. It treats `ZGRUND` as input for land/bank
!> elements and fills the link entries from the OC/cross-section setup.
!>
!> Initialised shared outputs include boundary-condition counts and codes
!> (`NOCHB`, `NOCFB`, `NOCBCC`, `NOCBCD`, `COCBCD`), hydraulic geometry
!> (`HRF`, `CWIDTH`, `ZBEFF`, `ZBFULL`, `NXSECT`, `XINH`, `XINW`, `XAREA`,
!> `XSTAB`), Strickler/roughness fields (`STRX`, `STRY`), timing controls
!> (`TDC`, `TFC`), and row-index arrays (`NELIND`, `NROWEL`, `NROWST`,
!> `NROWF`, `NROWL`) used by the OC implicit row solver.
SUBROUTINE OCINI()
! Locals, etc
!INTRINSIC MOD
INTEGER :: KONT
DOUBLEPRECISION DDUM1 (NOCTAB), DDUM2 (NOCTAB, NOCTAB)
LOGICAL :: LDUM1 (NELEE)
!
CALL OCCHK0()
!CALL OCCHK1 (ICMREF (1, 5), &
!CALL OCCHK1(ICMREF (1:4, 5), SIZE(ldum1), LDUM1) !AD aliasing
CALL OCCHK1(SIZE(ldum1), LDUM1) !AD aliasing
!
! Input data & associated requirements
!
CALL OCREAD(KONT, TDC, TFC, DDUM1, DDUM2)
CALL OCCHK2 (DUMMY, DDUM1, nelee, LDUM1)
!
! Boundary data files
!
!     Read title lines
IF (NOCHB.GT.0) READ (OHB, * )
IF (NOCFB.GT.0) READ (OFB, * )


CALL INITIALISE_OCMOD()

!
! Cross-section tables & effective bed elevations
!
IF (total_no_links.GT.0) THEN
   IF (MOD (KONT, 2) .EQ.1) WRITE(PPPRI, 9100) NXSCEE
   CALL OCXS()
ENDIF
!
! Indicies for Thomas algorithm
!
CALL OCIND(BEXBK, NROWF, NROWL, NROWST, NELIND, NROWEL)
 9100 FORMAT (/5X,'Size of internal tables for channel conveyance, etc', '  NXSCEE =',I6)
END SUBROUTINE OCINI



!> Assembles one element row of the implicit OC matrix.
!>
!> Given the current element, boundary type, water level, storage area, rainfall,
!> evaporation, exchange flow, and previously calculated flow derivatives,
!> `OCABC` fills the lower, central, upper, and right-hand-side coefficients
!> used by the row-wise implicit solver in [[OCSIM]].
!>
!> The routine uses the element topology in `ICMREF`, multi-link node expansion
!> in `ICMRF2`, flow derivatives `DQ0ST`, `DQIST`, and `DQIST2`, bank exchange
!> flows `QBKB` and `QBKF`, current face flows from `QSA`, row indices
!> `NELIND`, and channel cross-section tables `XINH`/`XINW`.
!>
!> For fixed-head boundary types `IBC=3` and `IBC=9`, the assembled row simply
!> enforces the prescribed head increment:
!>
!> \[
!> \Delta H = HNOW - H,\qquad BB_{IND}=1,\qquad FF=HNOW-H.
!> \]
!>
!> Otherwise the unknown is the water-level correction for the current element
!> and its neighbours. The current water depth is
!>
!> \[
!> H = Z - ZG.
!> \]
!>
!> For land/bank elements the storage area is `AREAE`. For channel links below
!> bankfull level, the storage width is linearly interpolated from the
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
!> Rainfall, evaporation, bank exchange, and external exchange are assembled on
!> the right-hand side as
!>
!> \[
!> FF = -AREAE\,(P_{net}+Q_H-E_{sw}) + Q_{bank},
!> \]
!>
!> with `Q_H=QHE` only for non-link elements. For links, rainfall is suppressed
!> when `H < 1D-8`, and
!>
!> \[
!> Q_{bank}=QBKB_{1}+QBKF_{1}+QBKB_{2}+QBKF_{2}.
!> \]
!>
!> Each face flow is taken positive into the current element. For face \(f\),
!> the previously calculated linearisation is applied as
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
SUBROUTINE OCABC(IND, IROW, ielz, NSV, NCR, NPR, IBC, N, AREAE, &
 ZG, CL, ZBF, Z, PNETT, QHE, ESWAE, HNOW, AA, BB, CC, FF)
INTEGER, INTENT(IN)         :: IND, IROW, IELz, NSV, NCR, NPR, IBC, N
DOUBLEPRECISION, INTENT(IN) :: AREAE, ZG, CL, ZBF, Z, PNETT, QHE, ESWAE, HNOW
DOUBLEPRECISION, INTENT(OUT) :: AA(NXOCEE), BB (NCR), CC (NXOCEE), FF
INTEGER :: I, IBR, IFACE, IM
INTEGER :: J, JEL, JFACE, JND, JROW
DOUBLEPRECISION AR, BKDUM, DQ0, DQI, H, HI, HM, PDUM, Q, QHDUM, WI, WM
LOGICAL :: BLINK, TEST, iscycle
!
! ----- INITIALIZE OUTPUT ARRAYS & GET WATER DEPTH
!
IF (NSV.GT.0) CALL ALINIT (ZERO, NSV, AA)
CALL ALINIT (ZERO, NCR, BB)
IF (NPR.GT.0) CALL ALINIT (ZERO, NPR, CC)
H = Z - ZG
!
! ----- HEAD BOUNDARY
!
IF ((IBC.EQ.3).OR.(IBC.EQ.9)) THEN
   BB (IND) = one
   FF = HNOW - H
   RETURN
ENDIF
!
! ----- IS THE CURRENT ELEMENT A LINK?
!
BLINK = ICMREF (ielz, 1) .EQ.3
!
! ----- PUT STORAGE TERM INTO CENTRAL COEFFICIENT FOR CURRENT ELEMENT
!
TEST = BLINK
IF (TEST) TEST = Z.LT.ZBF
IF (TEST) THEN
    !         * note requirements: XINH(IEL,1)=0; XINH(IEL,N).GE.ZBF-ZG
    iscycle=.FALSE.
    DO I = 2, N
        IF(iscycle) CYCLE
        HI = XINH (ielz, I)
        IF (H.LT.HI) THEN
            IM = I - 1
            HM = XINH (ielz, IM)
            WM = XINW (ielz, IM)
            WI = XINW (ielz, I)
            AR = CL * (WM + (WI - WM) * ( (H - HM) / (HI - HM) ) )
            iscycle = .TRUE. !GOTO 20
            CYCLE
        ENDIF
    ENDDO
ELSE
    AR = AREAE
ENDIF
BB (IND) = - AR / DTOC
!
! ----- PUT PRECIPITATION, EVAPORATION AND EXCHANGE FLOWS INTO RHS
!
PDUM = PNETT
IF (BLINK) THEN
   IF (H.LT.1D-8) PDUM = ZERO
   BKDUM = QBKB (ielz, 1) + QBKF (ielz, 1) + QBKB (ielz, 2) + QBKF ( &
    ielz, 2)
   QHDUM = ZERO
ELSE
   BKDUM = ZERO
   QHDUM = QHE
ENDIF
FF = - AREAE * (PDUM + QHDUM - ESWAE) + BKDUM
!
! ----- LOOP OVER ADJACENT ELEMENTS
!
DO 500 IFACE = 1, 4
   JEL = ICMREF (ielz, IFACE+4)
   JFACE = ICMREF (ielz, IFACE+8)
!
! --- GET FLOW AND DERIVATIVE (+VE INTO ELEMENT)
!
   Q = GETQSA (ielz, IFACE)
   DQ0 = DQ0ST (ielz, IFACE)
!
! --- ADD INTO COEFFICIENTS FOR CURRENT ELEMENT
!
   BB (IND) = BB (IND) + DQ0
   FF = FF - Q
!
! --- TEST FOR SINGLE ADJACENT ELEMENT
!
   IF (JEL.GT.0) THEN
!
      JROW = ICMREF (JEL, 3)
      JND = NELIND (JEL)
      DQI = DQIST (ielz, IFACE)
!
!        ADD DERIVATIVE TO COEFFICIENT FOR ADJACENT ELEMENT
!
      IF (JROW.EQ.IROW) BB (JND) = BB (JND) + DQI
      IF (JROW.GT.IROW) AA (JND) = AA (JND) + DQI
      IF (JROW.LT.IROW) CC (JND) = CC (JND) + DQI
!
! --- SIMILARLY FOR MULTIPLE ADJACENT LINKS
!
   ELSEIF (JEL.LT.0) THEN
!
      IBR = - JEL
      DO 200 J = 1, 3
         JEL = ICMRF2 (IBR, J)
         IF (JEL.GT.0) THEN
            JROW = ICMREF (JEL, 3)
            JND = NELIND (JEL)
            DQI = DQIST2 (IBR, J)
            IF (JROW.EQ.IROW) BB (JND) = BB (JND) + DQI
            IF (JROW.GT.IROW) AA (JND) = AA (JND) + DQI
            IF (JROW.LT.IROW) CC (JND) = CC (JND) + DQI
         ENDIF
  200       END DO
!
   ENDIF
  500 END DO
END SUBROUTINE OCABC



!> Reads and builds OC boundary-condition metadata.
!>
!> `JEOCBC` maps gridded head, flux, polynomial, channel-link, and impermeable
!> boundary-condition definitions onto `NOCBCD` and `NOCBCC`, including extra
!> bank elements where bank flow is represented.
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
!> On exit, `IXER` is only increased, every `NOCBCC(element)` is either zero or
!> a boundary-condition index, and a clean exit satisfies
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
!> Head boundaries use type 3 and no face (`face=0`), while flux and polynomial
!> boundaries use the element's stored boundary face `NBFACE(e)` with types 4
!> and 5. Polynomial boundary rows also receive the five coefficients read from
!> record `OC28`:
!>
!> \[
!> COCBCD_{1:5,b}=a_{1:5}(ICAT).
!> \]
!>
!> Channel-link boundary codes are taken directly from `LCODEX`/`LCODEY` when
!> their values are 7:11. The corresponding link is found with `LINKNO`, a row
!> is added with `NOCBCD(:,1)=link` and `NOCBCD(:,3)=type`, and type 9 and 10
!> entries increment the head-boundary and flux-boundary counts respectively.
!>
!> Internal impermeable grid boundaries use type 1. For each impermeable
!> west/south grid boundary, `JEOCBC` creates reciprocal rows for the two
!> adjacent elements and extends the impermeable condition across the ends of
!> any adjacent bank elements. The reciprocal face is taken from `ICMREF(:,9:12)`
!> so the table remains consistent with the topology built by [[frmod:frind]].
SUBROUTINE JEOCBC(IXER, NOCBC)
INTEGER, INTENT(OUT)         :: IXER
INTEGER, INTENT(OUT)         :: NOCBC
INTEGER                      :: BANK, I, IBANK, IBC, IBC0, IBK, ICAT, IELy, IFACE
INTEGER                      :: J, JBANK, JBC, JEL, K, KFACE, NOCPB, TYPEE
DOUBLEPRECISION              :: ADUM (5)
LOGICAL                      :: TEST
CHARACTER (LEN=77)           :: MSG
!----------------------------------------------------------------------*
!
! NUMBER OF CATEGORIES FOR EACH TYPE
!:OC20
READ (OCD, * )
READ (OCD, * ) NOCHB,NOCFB,NOCPB
!
! INITIALIZATION
!
NOCBC = 0
DO 10 iely = 1,total_no_elements
   NOCBCC (iely) = 0
   10 END DO
!
! HEAD BOUNDARY (TYPE 3)
!:OC22
IF (NOCHB.GT.0) THEN
   MSG = 'ERROR IN OC HEAD BOUNDARY GRID'
   CALL AREADI (IDUM, 0, OCD, PPPRI, NOCHB)
   DO iely = NGDBGN,total_no_elements
      ICAT = IDUM (iely)
      IF ((ICAT.LT.0).OR.(ICAT.GT.NOCHB)) THEN
         IXER = IXER + 1
         CALL ERROR(EEERR, 1020, PPPRI, iely, 0, MSG)
      ELSEIF (ICAT.GT.0) THEN
         NOCBC = NOCBC + 1
         IF (NOCBC.GT.NOCTAB) CYCLE
         NOCBCC (iely) = NOCBC
         NOCBCD (NOCBC, 1) = iely
         NOCBCD (NOCBC, 2) = 0
         NOCBCD (NOCBC, 3) = 3
         NOCBCD (NOCBC, 4) = ICAT
      ENDIF
    ENDDO
ENDIF
!
! FLUX BOUNDARY (TYPE 4)
!:OC24
IF (NOCFB.GT.0) THEN
   MSG = 'ERROR IN OC FLUX BOUNDARY GRID'
   CALL AREADI (IDUM, 0, OCD, PPPRI, NOCFB)
   DO 40 iely = NGDBGN,total_no_elements
      ICAT = IDUM (iely)
      IF ((ICAT.LT.0).OR.(ICAT.GT.NOCFB)) THEN
         IXER = IXER + 1
         CALL ERROR(EEERR, 1021, PPPRI, iely, 0, MSG)
      ELSEIF (ICAT.GT.0) THEN
         NOCBC = NOCBC + 1
         IF (NOCBC.GT.NOCTAB) CYCLE
         NOCBCC (iely) = NOCBC
         NOCBCD (NOCBC, 1) = iely
         NOCBCD (NOCBC, 2) = NBFACE (iely)
         NOCBCD (NOCBC, 3) = 4
         NOCBCD (NOCBC, 4) = ICAT
      ENDIF
   40    END DO
ENDIF
!
! POLYNOMIAL FUNCTION BOUNDARY (TYPE 5)
!:OC26
IF (NOCPB.GT.0) THEN
   IBC0 = NOCBC
   MSG = 'ERROR IN OC POLYNOMIAL FUNCTION BOUNDARY GRID'
   CALL AREADI (IDUM, 0, OCD, PPPRI, NOCPB)
   DO 60 iely = NGDBGN,total_no_elements
      ICAT = IDUM (iely)
      IF ((ICAT.LT.0).OR.(ICAT.GT.NOCPB)) THEN
         IXER = IXER + 1
         CALL ERROR(EEERR, 1022, PPPRI, iely, 0, MSG)
      ELSEIF (ICAT.GT.0) THEN
         NOCBC = NOCBC + 1
         IF (NOCBC.GT.NOCTAB) CYCLE
         NOCBCC (iely) = NOCBC
         NOCBCD (NOCBC, 1) = iely
         NOCBCD (NOCBC, 2) = NBFACE (iely)
         NOCBCD (NOCBC, 3) = 5
         NOCBCD (NOCBC, 4) = ICAT
      ENDIF
   60    END DO
!:OC28
   MSG = 'Error reading polynomial function data in OC'
   READ (OCD, * )
   DO 80 I = 1, NOCPB
      READ (OCD, * ) ICAT, ADUM
      IF (ICAT.NE.I) THEN
         IXER = IXER + 1
         CALL ERROR(EEERR, 1031, PPPRI, iely, 0, MSG)
      ELSE
         DO 70 IBC = IBC0 + 1, MIN (NOCBC, NOCTAB)
            TEST = NOCBCD (IBC, 4) .EQ.I
            !IF (TEST) CALL DCOPY(5, ADUM, 1, COCBCD(1:5,IBC), 1)
            IF (TEST) COCBCD(1:5,IBC) = adum
   70          END DO
      ENDIF
   80    END DO
ENDIF
!
! SET CHANNEL LINK BOUNDARY TYPES (other data will follow)
!
DO 100 I = 1, NX
   DO 100 J = 1, NY
      DO 90 K = 0, 1
         TYPEE = LCODEX (I, J) * (1 - K) + LCODEY (I, J) * K
         IF ((TYPEE.GE.7).AND.(TYPEE.LE.11)) THEN
            iely = LINKNO (I, J, K.EQ.0)
            NOCBC = NOCBC + 1
            IF (NOCBC.LE.NOCTAB) THEN
               NOCBCC (iely) = NOCBC
               NOCBCD (NOCBC, 1) = iely
               NOCBCD (NOCBC, 3) = TYPEE
               IF (TYPEE.EQ.9) NOCHB = NOCHB + 1
               IF (TYPEE.EQ.10) NOCFB = NOCFB + 1
            ENDIF
         ENDIF
   90       END DO
  100 CONTINUE
!
! SET INTERNAL IMPERMEABLE GRID BOUNDARY CONDITIONS (TYPE 1)
!
! NB Impermeability extended across ends of any adjacent bank elements
!
IBC0 = NOCBC
DO 110 I = 1, NX
   DO 110 J = 1, NY
      DO 107 IFACE = 3, 4
         TYPEE = LCODEX (I, J) * (4 - IFACE) + LCODEY (I, J) &
          * (IFACE-3)
         IF (TYPEE.EQ.1) THEN
            iely = ICMXY (I, J)
            JEL = 0
            IF (iely.GT.0) JEL = ICMREF (iely, 4 + IFACE)
            IF (JEL.GT.0) THEN
               NOCBC = NOCBC + 1
               IF (NOCBC.LE.NOCTAB) THEN
                  NOCBCC (iely) = NOCBC
                  NOCBCD (NOCBC, 1) = iely
                  NOCBCD (NOCBC, 2) = IFACE
               ENDIF
               NOCBC = NOCBC + 1
               IF (NOCBC.LE.NOCTAB) THEN
                  NOCBCC (JEL) = NOCBC
                  NOCBCD (NOCBC, 1) = JEL
                  NOCBCD (NOCBC, 2) = ICMREF (iely, 8 + IFACE)
               ENDIF
               DO 104 BANK = 2, 1, - 1
                  KFACE = 9 - IFACE-2 * BANK
                  IBANK = ICMREF (iely, 4 + KFACE)
                  IBK = 0
                  IF (IBANK.GT.0) IBK = ICMREF (IBANK, 1)
                  IF (IBK.EQ.BANK) THEN
                     NOCBC = NOCBC + 1
                     IF (NOCBC.LE.NOCTAB) THEN
                        NOCBCC (IBANK) = NOCBC
                        NOCBCD (NOCBC, 1) = IBANK
                        NOCBCD (NOCBC, 2) = IFACE
                     ENDIF
                     NOCBC = NOCBC + 1
                     IF (NOCBC.GT.NOCTAB) CYCLE
                     JBANK = ICMREF (IBANK, 4 + IFACE)
                     NOCBCC (JBANK) = NOCBC
                     NOCBCD (NOCBC, 1) = JBANK
                     NOCBCD (NOCBC, 2) = ICMREF (IBANK, 8 + IFACE)
                  ENDIF
  104                END DO
            ENDIF
         ENDIF
  107       END DO
  110 CONTINUE
DO 120 IBC = IBC0 + 1, MIN (NOCBC, NOCTAB)
   NOCBCD (IBC, 3) = 1
   NOCBCD (IBC, 4) = 1
  120 END DO
!
! CHECK
!
IF (NOCBC.GT.NOCTAB) THEN
   IXER = IXER + 1
   WRITE (MSG, 9050) NOCBC, NOCTAB
   CALL ERROR(EEERR, 1050, PPPRI, 0, 0, MSG)
ENDIF
DO 1000 IBC = 1, MIN (NOCBC, NOCTAB)
   iely = NOCBCD (IBC, 1)
   JBC = NOCBCC (iely)
   IF (JBC.NE.IBC) THEN
      IXER = IXER + 1
      WRITE (MSG, 9100) NOCBCD (IBC, 3), NOCBCD (JBC, 3)
      CALL ERROR(EEERR, 1059, PPPRI, iely, 0, MSG)
   ENDIF
 1000 END DO
!
! FINISH
!
 9050 FORMAT ('Number of OC boundary conditions NOCBC =',I4,2X, &
&        'exceeds array size NOCTAB =',I4)

 9100 FORMAT ('Element has multiple OC boundary conditions ', &
&        '(types',I2,' and',I2,')')
END SUBROUTINE JEOCBC



!> Checks OC file units, array bounds, and global entity counts.
!>
!> This is the first OC validation pass and ensures the output/input units are
!> usable and that compiled dimensions are large enough for the current grid,
!> channel-link, cross-section, and boundary-condition counts.
SUBROUTINE OCCHK0()
!----------------------------------------------------------------------*
!
!  Check static variables & constants input to the OC
!
!----------------------------------------------------------------------*
INTEGER       :: ERRNUM, I, IUNDEF, IUNIT, NERR, OUNIT
INTEGER       :: IDUMS (1), IDUMO (1)
LOGICAL       :: BOPEN, LDUM1 (1)
CHARACTER(47) :: MSG
CHARACTER(11) :: FORM
CHARACTER(3)  :: NAME
nerr = 0
!----------------------------------------------------------------------*
! 1. Unit Numbers
! ---------------
!PRI, OCD
OUNIT = PPPRI
IUNIT = PPPRI
NAME = 'PRI'
DO  I = 0, 1
   INQUIRE (IUNIT, OPENED = BOPEN, FORM = FORM)
   IF (.NOT.BOPEN) THEN
      WRITE (MSG, 9100) NAME, IUNIT, 'is not connected to a file'
      ERRNUM = 1008
   ELSEIF (FORM.NE.'FORMATTED') THEN
      WRITE (MSG, 9100) NAME, IUNIT, 'has format type', FORM
      ERRNUM = 1009
   ELSE
      GOTO 110
   ENDIF
   IF (I.EQ.0) OUNIT = 0
   CALL ERROR(EEERR, ERRNUM, OUNIT, 0, 0, MSG)
   NERR = NERR + 1
  110    IUNIT = OCD
   NAME = 'OCD'
ENDDO
IDUMS (1) = MIN (PPPRI, OCD)

CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ PRI, OCD ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)
! 2. Array Sizes
! --------------
!NELEE
IDUMS (1) = NELEE
IDUMO (1) = MAX (NX, total_no_elements)! , NOCTAB*NOCTAB)
CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
!NLFEE
IDUMS (1) = NLFEE
IDUMO (1) = MAX (1, total_no_links)
CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
!NXEE
IDUMS (1) = NXEE
IDUMO (1) = NX
CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
!NOCTAB
IDUMS (1) = NOCTAB
CALL ALCHKI (EEERR, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NOCTAB', 'GE', IONE1, IDUMS, NERR, LDUM1)
!NXSCEE
IDUMS (1) = NXSCEE

CALL ALCHKI (EEERR, 1002, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXSCEE', 'GT', IONE1, IDUMS, NERR, LDUM1)
! 3. Number of Entities
! ---------------------
!NLF
IDUMS (1) = total_no_links
CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', IZERO1, IDUMS, NERR, LDUM1)
IDUMO (1) = total_no_elements
CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', IDUMO, IDUMS, NERR, LDUM1)
!NX, NY
IDUMS (1) = MIN (NX, NY)
CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ NX, NY ]', 'GE', IONE1, IDUMS, NERR, LDUM1)
!NGDBGN
IDUMS (1) = NGDBGN
IDUMO (1) = total_no_links + 1


CALL ALCHKI (EEERR, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NGDBGN', 'EQ', IDUMO, IDUMS, NERR, LDUM1)
! 4. Finish
! ---------
IF (NERR.GT.0) THEN
    CALL ERROR(FFFATAL, 1000, OUNIT, 0, 0, 'Error(s) detected while checking OC input variables & constants')
ENDIF
 9100 FORMAT ('File unit ',A,' =',I4,1X,A:1X,A)
END SUBROUTINE OCCHK0



!SUBROUTINE OCCHK1(afromICMREF, SZLOG, LDUM1)
!> Checks static OC topology and channel-definition arrays.
!>
!> `OCCHK1` verifies neighbour references, active-grid indexing, and the
!> link-code grids used to locate north-south and east-west channel links.
SUBROUTINE OCCHK1(SZLOG, LDUM1)
!----------------------------------------------------------------------*
!
!  Check static OC input arrays
!
!----------------------------------------------------------------------*
! Entry requirements:
!  [ NEL, NX, NY ].ge.1     NELEE.ge.NEL    NXEE.ge.NX
!  PRI open for F output    size_of_LDUM1.ge.[NX,NEL]
!----------------------------------------------------------------------*
INTEGER, INTENT(IN) :: szlog
!INTEGER, INTENT(inout) :: afromICMREF(4)  !AD aliasing
LOGICAL, INTENT(OUT):: LDUM1 (szlog)  !LDUM1 ( * )
INTEGER             :: CODE, FACE, I, IELx, IUNDEF, NERR, TYPEE, X, Y
INTEGER             :: IDUMO (1)
CHARACTER(23)       :: NAME
CHARACTER           :: XY (0:1)
DATA NERR / 0 / , XY / 'X', 'Y' /
!----------------------------------------------------------------------*
! 1. Index Arrays
! ---------------
IDUMO (1) = total_no_elements
!ICMREF
DO 110 FACE = 1, 4
CALL ALCHKI (EEERR, 1057, PPPRI, 1, total_no_elements, FACE, 2, 'ICMREF(iel,face,2)' &
!&, 'LE', IDUMO, ICMREF (1, FACE, 2) , NERR, LDUM1)
!&, 'LE', IDUMO, afromICMREF(FACE) , NERR, LDUM1(1:NEL))
&, 'LE', IDUMO, ICMREF(1:total_no_elements, 4+FACE) , NERR, LDUM1(1:total_no_elements))
  110 END DO
!ICMXY
DO 120 Y = 1, NY
   CALL ALCHKI (EEERR, 1057, PPPRI, 1, NX, Y, IUNDEF, 'ICMXY(x,y)', &
    'LE', IDUMO, ICMXY (1, Y) , NERR, LDUM1(1:NX))
  120 END DO
! 2. Channel Definition Arrays
! ----------------------------
!LCODEX,LCODEY
NAME = 'validity_of_LCODE?(x,y)'
DO 230 I = 0, 1
   NAME (18:18) = XY (I)
   DO 220 Y = 1, NY
      DO 210 X = 1, NX
         CODE = 0
         TYPEE = LCODEX (X, Y) * (1 - I) + LCODEY (X, Y) * I
         IF ((TYPEE.GE.7).AND.(TYPEE.LE.11)) THEN
            ielx = LINKNO (X, Y, I.EQ.0)
            IF (ielx.LE.0.OR.ielx.GE.NGDBGN) CODE = TYPEE
         ENDIF
         IDUM (X) = CODE
  210       END DO
      CALL ALCHKI (EEERR, 1058, PPPRI, 1, NX, Y, IUNDEF, NAME, 'EQ', &
       IZERO1, IDUM, NERR, LDUM1(1:NX))
  220    END DO
  230 END DO
! 3. Finish
! ---------

IF (NERR.GT.0) CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking static OC input arrays')
END SUBROUTINE OCCHK1



!> Checks OC input values after [[OCREAD]].
!>
!> The checks cover boundary file units, overland/channel roughness values, and
!> channel cross-section tables, including monotonic level coordinates and
!> positive final widths.
SUBROUTINE OCCHK2 (DDUM1A, DDUM1B, SZLOG, LDUM1)
!----------------------------------------------------------------------*
!
!  Check OC input data
!
!----------------------------------------------------------------------*
! Entry requirements:
!  NEL.ge.[NLF,1]    NLFEE.ge.[NLF,1]    PRI open for F output
!----------------------------------------------------------------------*
INTEGER, INTENT(IN)           :: szlog
DOUBLEPRECISION, INTENT(OUT) :: DDUM1A (:), DDUM1B(:)
LOGICAL, INTENT(IN)          :: LDUM1(szlog)  !LDUM1 ( * )
INTEGER                      :: ERRNUM, I, IELw, IUNDEF, IUNIT, N, NERR
INTEGER                      :: IDUMS (1)
LOGICAL                      :: BOPEN, NONEED
CHARACTER (47)               :: MSG
CHARACTER(11)                :: FORM
CHARACTER(3)                 :: NAME
CHARACTER(19)                :: SUBJ
DATA NERR / 0 /
!----------------------------------------------------------------------*
! 1. Unit Numbers
! ---------------
!OHB,OFB
IDUMS (1) = 0
IUNIT = OHB
NAME = 'OHB'
NONEED = NOCHB.EQ.0

DO I = 0, 1
   IF (NONEED) GOTO 110

   IDUMS (1) = MIN (IUNIT, IDUMS (1) )
   INQUIRE (IUNIT, OPENED = BOPEN, FORM = FORM)
   IF (.NOT.BOPEN) THEN
      WRITE (MSG, 9100) NAME, IUNIT, 'is not connected to a file'
      ERRNUM = 1008
   ELSEIF (FORM.NE.'FORMATTED') THEN
      WRITE (MSG, 9100) NAME, IUNIT, 'has format type', FORM
      ERRNUM = 1009
   ELSE
      GOTO 110
   ENDIF
   CALL ERROR(EEERR, ERRNUM, PPPRI, 0, 0, MSG)

   NERR = NERR + 1
  110    IUNIT = OFB
   NAME = 'OFB'

   NONEED = NOCFB.EQ.0
END DO
CALL ALCHKI (EEERR, 1003, PPPRI, 1, 1, IUNDEF, IUNDEF, '[ OHB, OFB ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)
! 2. Element Properties
! ---------------------
!STRX

CALL ALCHK(EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRX(iel)', 'GT', ZERO1, ZERO, STRXX, NERR, LDUM1)
!STRY
CALL ALCHK (EEERR, 1010, PPPRI, 1, total_no_elements, IUNDEF, IUNDEF, 'STRY(iel)', 'GT', zero1, zero, STRYY, NERR, LDUM1)
! 3. Cross-section Tables
! -----------------------
!
IF (total_no_links.GT.0) THEN
!XINH
CALL ALCHK (EEERR, 1016, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINH(link)[j=1]', 'EQ', zero1, zero , XINH, NERR, LDUM1)
   DO 310 ielw = 1, total_no_links
      N = NXSECT (ielw) - 1
      WRITE (SUBJ, 9310) ielw
      !CALL DCOPY(N, XINH(IEL,1), NLFEE, DDUM1A(1:n), 1)
      !CALL DCOPY(N, XINH(IEL, 2), NLFEE, DDUM1B(1:n), 1)
      DDUM1A(1:n) = XINH(IELw,1:n)
      DDUM1B(1:n) = XINH(ielw, 2:n+1)
      CALL ALCHK (EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, 'GTa', DDUM1A, zero , DDUM1B, NERR, LDUM1)
!XINW
      SUBJ (4:4) = 'W'
      !CALL DCOPY(N, XINW(IEL,1), NLFEE, DDUM1A(1:n), 1)
      !CALL DCOPY(N, XINW(IEL,2), NLFEE, DDUM1B(1:n), 1)
      DDUM1A(1:n) = XINW(ielw,1:n)
      DDUM1B(1:n) = XINW(ielw,2:n+1)
      CALL ALCHK (EEERR, 1017, PPPRI, 1, N, IUNDEF, IUNDEF, SUBJ, &
       'GEa', DDUM1A, zero , DDUM1B, NERR, LDUM1)
  310    END DO
   DO 320 ielw = 1, total_no_links
      DDUM1A (ielw) = XINW (ielw, NXSECT (ielw) )
  320    END DO
CALL ALCHK (EEERR, 1056, PPPRI, 1, total_no_links, IUNDEF, IUNDEF, 'XINW[link,NXSECT(link)]', 'GT', zero1, zero , &
                   DDUM1A, NERR, LDUM1)
ENDIF

IF (NERR.GT.0) then
! sb 190522 negative strickler for surface storage
    CALL ERROR(WWWARN, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
!    CALL ERROR(FFFATAL, 1000, PPPRI, 0, 0, 'Error(s) detected while checking OC input data')
ENDIF
 9100 FORMAT ('File unit ',A,' =',I4,1X,A:1X,A)

 9310 FORMAT ('XINH[ link =',I3,'](j)')
END SUBROUTINE OCCHK2



!> Reads time-varying head and flux boundary values for the current OC step.
!>
!> Boundary time series are advanced from the head-boundary and flux-boundary
!> files into `HOCNOW` and `QOCF`; end-of-file markers are treated as fatal
!> input errors.
SUBROUTINE OCEXT
!----------------------------------------------------------------------
!
! READ IN TIME-VARYING BOUNDARY CONDITION DATA
!----------------------------------------------------------------------
!
! HEAD BOUNDARY
!
IF (NOCHB.GT.0) CALL HINPUT (OHB, TIH, OCNOW, OCNEXT, HOCLST, HOCNXT, HOCPRV(1:nochb), HOCNXV(1:nochb), NOCHB, HOCNOW(1:nochb))
IF (EQMARKER(HOCNXT)) CALL ERROR(FFFATAL, 1007, PPPRI, 0, 0, 'END OF OC HEAD BOUNDARY DATA')
!
! FLUX BOUNDARY
!
IF (NOCFB.GT.0) CALL FINPUT (OFB, TIH, OCNOW, OCNEXT, QFLAST, QFNEXT, QOCFIN(1:nocfb), NOCFB, QOCF(1:nocfb))
IF (EQMARKER(QFNEXT)) CALL ERROR(FFFATAL, 1023, PPPRI, 0, 0, 'END OF OC FLUX BOUNDARY DATA')
!
RETURN
END SUBROUTINE OCEXT



!> Builds row-order indexing for the implicit OC solver.
!>
!> The catchment is split into y-coordinate rows, with west/south channel links
!> and optional bank elements inserted beside the grid elements. The resulting
!> `NROWST`, `NROWEL`, and `NELIND` arrays define the block rows used by
!> [[OCSIM]].
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
!> For each grid square `(i,j)`, `OCIND` scans the west face (`FACE=3`) and then
!> the south face (`FACE=4`). If a link is present, the row receives
!>
!> | Bank option | Inserted sequence |
!> |:------------|:------------------|
!> | `BEXBK=.FALSE.` | `link` |
!> | `BEXBK=.TRUE.` | bank on one side, `link`, bank on the other side |
!>
!> using `ICMBK(link,5-FACE)` before the link and `ICMBK(link,FACE-2)` after
!> the link. On the west-face pass only, the active grid element `ICMXY(i,j)` is
!> then inserted. Thus the current row length is
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
!>
SUBROUTINE OCIND(BEXBK, NROWF, NROWL, NROWST, NELIND, NROWEL)
LOGICAL, INTENT(IN)  :: BEXBK
INTEGER, INTENT(OUT) :: NROWF, NROWL, NROWST (NY + 1), NELIND(:), NROWEL(:)
INTEGER              :: BANK, FACE, I, ICOUNT, IELv, J, K, LINK, NXOC
!----------------------------------------------------------------------*
!
! Initialize counters
!
NXOC = 0
K = 0
!
! LOOP OVER BASIC GRID SYSTEM
!
! - LOOP OVER EACH ROW
!

DO 20 J = 1, NY
   NROWST (J) = K + 1
   IF (K.EQ.0) NROWF = J
!
! ---- LOOP OVER EACH GRID SQUARE IN ROW
!
   ICOUNT = 0
   DO 10 I = 1, NX
!
! ------- Loop over west & south faces
!
      DO 5 FACE = 3, 4
!
! ---------- Test for link at face of grid
!
         LINK = LINKNO (I, J, FACE.EQ.3)
         IF (LINK.GT.0) THEN
            IF (BEXBK) THEN
               BANK = ICMBK (LINK, 5 - FACE)
               K = K + 1
               ICOUNT = ICOUNT + 1
               NROWEL (K) = BANK
               NELIND (BANK) = ICOUNT
            ENDIF
            K = K + 1
            ICOUNT = ICOUNT + 1
            NROWEL (K) = LINK
            NELIND (LINK) = ICOUNT
            IF (BEXBK) THEN
               BANK = ICMBK (LINK, FACE-2)
               K = K + 1
               ICOUNT = ICOUNT + 1
               NROWEL (K) = BANK
               NELIND (BANK) = ICOUNT
            ENDIF
         ENDIF
!
! ---------- Test for active grid square
!
         IF (FACE.EQ.3) THEN
            ielv = ICMXY (I, J)
            IF (ielv.GT.0) THEN
               K = K + 1
               ICOUNT = ICOUNT + 1
               NROWEL (K) = ielv
               NELIND (ielv) = ICOUNT
            ENDIF
         ENDIF
!
! ---------- Next face
!
    5       END DO
!
! ------- Next square
!
   10    END DO
!
! ---- Next row
!
   NXOC = MAX (NXOC, K + 1 - NROWST (J) )
   IF (ICOUNT.GT.0) NROWL = J
!
   20 END DO
!
! - This marks the end of the last row (+1)
!
NROWST (J) = K + 1
!
! CHECK ARRAY DIMENSIONS
!
IF (NXOC.GT.NXOCEE) THEN
CALL ERROR(FFFATAL, 1006, PPPRI, 0, 0, 'ARRAY DIMENSION OF NXOC TOO SMALL')
ENDIF
!
END SUBROUTINE OCIND



! 12/8/94
!> Reads an alphanumeric channel-definition grid.
!>
!> `OCLTL` decodes the legacy one-character OC map into integer link and
!> boundary codes, preserving row-number checks and optional echo printing.
SUBROUTINE OCLTL (NNX, NNY, IARR, NXE, NYE, INF, IOF, BPCNTL)
!
! READ IN ARRAY OF ALPHANUMERIC CODES FOR CHANNEL DEFINITION
!
! INPUT PARAMETERS:
!   NNX     X DIMENSION OF GRID
!   NNY     Y DIMENSION OF GRID
!   NXE     X DIMENSION OF ARRAY
!   NYE     Y DIMENSION OF ARRAY
!   INF     INOUT FILE UNIT NUMBER
!   IOF     OUTPUT FILE UNIT NUMBER
!   BPCNTL  LOGICAL PRINT CONTROL
!
! OUTPUT PARAMETERS:
!   IARR    ARRAY OF CODES READ IN FROM FILE
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
CHARACTER (LEN=80)  :: TITLE
CHARACTER (LEN=1)   :: CODES (11), A1LINE (500)
INTEGER, INTENT(IN) ::  NNX, NNY, NXE, NYE, INF, IOF
INTEGER             :: IARR (NXE, NYE), i, j, k, L, m
LOGICAL, INTENT(IN) :: BPCNTL
LOGICAL             :: iscycle, iscycle40
DATA CODES / 'I', '.', ' ', ' ', ' ', 'R', 'W', 'A', 'H', 'F', 'P' /
!
READ (INF, 10) TITLE
   10 FORMAT (A80)
IF (BPCNTL) WRITE (IOF, 20) TITLE
   20 FORMAT (A80)
!
DO 30 J = 1, NNY
   DO 30 I = 1, NNX
   30 IARR (I, J) = 0
!
I = NNY
iscycle40 = .FALSE.
DO 40 J = 1, NNY
    IF(iscycle40) CYCLE
    READ (INF, 50) K, (A1LINE (L), L = 1, NNX)
    50 FORMAT   (I7, 1X, 500A1)
    IF (BPCNTL) WRITE (IOF, 50) K, (A1LINE (L), L = 1, NNX)
    !
    IF (K.NE.I) THEN !GOTO 100
        iscycle40=.TRUE.
        CYCLE
    ENDIF
    I = I - 1

    DO L = 1, NNX   !70
        iscycle=.FALSE.
        DO M = 1, 11
            IF(iscycle) CYCLE
            !AD? IF (A1LINE (L) .EQ.CODES (M) .AND.CODES (M) .NE.' ') THEN
            IF ((A1LINE (L)==CODES(M)) .AND. (CODES(M)/=' ')) THEN
                IARR (L, K) = M
                iscycle=.TRUE. !GOTO 70
            ENDIF
        ENDDO
    ENDDO  !70
    !
40 END DO
IF(.NOT. iscycle40) RETURN
!
  100 IF (BPCNTL) WRITE (IOF, 110)
  110 FORMAT ('  ^^^   INCORRECT COORDINATE')
STOP
END SUBROUTINE OCLTL



!> Reads per-link channel geometry and link boundary data.
!>
!> `OCPLF` reads default and explicit cross-section definitions, link bed
!> elevations, initial water depths, Strickler roughness coefficients, and
!> boundary-condition parameters for river-link boundary types.
SUBROUTINE OCPLF(BOUT, IXER, fromNOCBCD, NXDEF, XDEFW)
!----------------------------------------------------------------------*
!
!  READ IN DATA FOR EACH CHANNEL LINK
!
!----------------------------------------------------------------------*
! Entry requirements:
!  NLFEE.ge.NLF    [NLF,NOCTAB].ge.1    NOCBCC(1:NLF).le.NOCTAB
!  OCD open for F input                 PRI open for F output
!----------------------------------------------------------------------*
! Exit conditions:
! IXER(out).ge.IXER(in)
! IXER(out).eq.IXER(in) ==> 2.le.NXSECT(1:NLF).le.NOCTAB
!----------------------------------------------------------------------*
LOGICAL, INTENT(INOUT) :: BOUT
INTEGER, INTENT(INOUT) :: IXER
INTEGER, INTENT(INOUT) :: fromNOCBCD(NOCTAB, 2:4)
INTEGER, INTENT(OUT)   :: NXDEF (NOCTAB)
DOUBLEPRECISION        :: XDEFH (NOCTAB, NOCTAB), XDEFW (NOCTAB, NOCTAB)
INTEGER                :: I, IBC, IDEF, IDEFX, ielm, J, N, NDEFCT, TYPEE
DOUBLEPRECISION        :: STR, WDEPTH, ZG
LOGICAL                :: TEST, g8055, g8013, g8300, greturn
CHARACTER(102)         :: MSG
!----------------------------------------------------------------------*
!
! READ DEFAULT CHANNEL CROSS-SECTIONS
!:OC30
READ (OCD, * )
READ (OCD, * ) NDEFCT
IF ((NDEFCT.GT.NOCTAB).OR.(NDEFCT.LT.0)) THEN !GOTO 8054
    WRITE (MSG, 9054) NDEFCT, NOCTAB
    CALL ERROR(EEERR, 1054, PPPRI, 0, 0, MSG)
    IXER = IXER + 1
ENDIF

g8013=.FALSE.
g8055=.FALSE.
g8300=.FALSE.
greturn=.FALSE.

!:OC32
IF (NDEFCT.GT.0) THEN
    READ (OCD, * )
    IF (BOUT) WRITE(PPPRI, 9032) 'Category', 'Width', 'Height'
    out100 : DO IDEF = 1, NDEFCT
        IF(g8055) CYCLE out100
        READ (OCD, * ) N
        IF ((N.GT.NOCTAB).OR.(N.LT.2)) THEN
            g8055=.TRUE.
            CYCLE out100
        ENDIF
        NXDEF (IDEF) = N
        READ (OCD, * ) (XDEFW (IDEF, J), XDEFH (IDEF, J), J = 1, N)
        IF (BOUT) WRITE(PPPRI, 9034) IDEF, (XDEFW (IDEF, J), XDEFH ( IDEF, J), J = 1, N)
    ENDDO out100
ENDIF
!
! READ DATA FOR EACH LINK
!:OC35
IF(g8055) THEN
    WRITE (MSG, 9055) IDEF, N, NOCTAB
    CALL ERROR(EEERR, 1055, PPPRI, 0, 0, MSG)
    IXER = IXER + 1
ELSE
    READ (OCD, * )
    IF (BOUT) WRITE(PPPRI, 9035) 'Element', 'Elevation', 'Init.Depth', 'Strickler', 'Width', 'Height'
    out500 : DO ielm = 1, total_no_links
        IF(g8013 .OR. g8300 .OR. greturn) CYCLE out500
        READ (OCD, *, ERR = 8300, END = 8300) I, ZG, WDEPTH, STR, IDEFX

        GOTO 877
        8300 g8300=.TRUE.
        CYCLE out500

        877 IF (I.NE.ielm) THEN
            g8013=.TRUE.
            CYCLE out500
        ENDIF

        ZGRUND (ielm) = ZG
        CALL SETHRF(ielm, ZG + WDEPTH)
        STRXX(ielm) = STR
        STRYY(ielm) = STR
        !:OC37
        TEST = (IDEFX.EQ.1).OR.(IDEFX.GT.NOCTAB)
        IF ((IDEFX.EQ.0).OR.(IDEFX.LT.-NDEFCT).OR.TEST) THEN
            WRITE (MSG, 9012) IDEFX, - NDEFCT, NOCTAB
            CALL ERROR(EEERR, 1012, PPPRI, ielm, 0, MSG)
            IXER = IXER + 1
            IF (TEST) THEN
                greturn=.TRUE.
                 CYCLE out500
            ENDIF
            GOTO 300
        ELSEIF (IDEFX.GT.0) THEN
            N = IDEFX
            READ (OCD, * ) (XINW (ielm, J), XINH (ielm, J), J = 1, N)
            IF (BOUT) WRITE(PPPRI, 9037) ielm, ZG, WDEPTH, STR, (XINW ( &
            ielm, J), XINH (ielm, J), J = 1, N)
        ELSE
            IDEF = - IDEFX
            N = NXDEF (IDEF)
            !CALL DCOPY(N, XDEFH(IDEF,1), NOCTAB, XINH(ielm,1), NLFEE)
            !CALL DCOPY(N, XDEFW(IDEF,1), NOCTAB, XINW(ielm,1), NLFEE)
            XINH(ielm,1:n) = XDEFH(IDEF,1:n)
            XINW(ielm,1:n) = XDEFW(IDEF,1:n)
            IF (BOUT) WRITE(PPPRI, 9137) ielm, ZG, WDEPTH, STR, IDEF
        ENDIF

        NXSECT (ielm) = N
        !
        ! CHANNEL BANK-FULL WIDTH & ELEVATION
        !
        CWIDTH (ielm) = XINW (ielm, N)

        ZBFULL (ielm) = XINH (ielm, N) + ZG
        !
        ! READ IN ADDITIONAL DATA FOR BOUNDARY CONDITIONS
        !:OC38-41
        300    IBC = NOCBCC (ielm)
        IF (IBC.GT.0) THEN
            TYPEE = fromNOCBCD (IBC, 3)
            IF((TYPEE.EQ.7).OR.(TYPEE.EQ.8)) THEN
                READ (OCD, * ) fromNOCBCD (IBC, 2), (COCBCD (J, IBC), J = 1, 4)
                fromNOCBCD (IBC, 4) = 1
            ELSEIF (TYPEE.EQ.9) THEN
                fromNOCBCD (IBC, 2) = 0
                READ (OCD, * ) fromNOCBCD (IBC, 4)
            ELSEIF (TYPEE.EQ.10) THEN
                READ (OCD, * ) (fromNOCBCD (IBC, J), J = 2, 4, 2)
            ELSEIF (TYPEE.EQ.11) THEN
                READ (OCD, * ) fromNOCBCD (IBC, 2), (COCBCD (J, IBC), J = 1, 5)
                fromNOCBCD (IBC, 4) = 1
            ENDIF
        ENDIF
    ENDDO out500
ENDIF
IF(greturn) THEN
    RETURN
ELSEIF(g8013) THEN
    WRITE (MSG, 9013) ielm, I
    CALL ERROR(EEERR, 1013, PPPRI, ielm, 0, MSG)
    IXER = IXER + 1
ELSEIF(g8300) THEN
    MSG = 'Channel input data is missing or has incorrect format'
    CALL ERROR(EEERR, 1019, PPPRI, ielm, 0, MSG)
    IXER = IXER + 1
ENDIF
 9012 FORMAT ('Cross-section number IDEFX =',I4,' lies outside ranges', &
&        ' -NDEFCT:-1 =',I4,' : -1  and  2:NOCTAB = 2 :',I4)

 9013 FORMAT ('Expected element number,'I5,', but found',I5,', ', &
&        'while reading channel data')

 9032 FORMAT (/5X,'Default Channel Cross-sections:'//5X,3A10/)

 9034 FORMAT (5X,I10,(T16,2F10.3))

 9035 FORMAT (/5X,'Link Element Data:'//5X,6A11/)

 9037 FORMAT (5X,I11,3F11.3,(T50,2F11.3))

 9054 FORMAT ('Number of default channel cross-section categories ', &
&        'NDEFCT =',I4,2X,'lies outside range 0:NOCTAB = 0 :',I4)

 9055 FORMAT ('Number of width/elevation pairs NXDEF(',I3,') =',I4,2X, &
&        'lies outside range 2:NOCTAB = 2:',I4)

 9137 FORMAT (5X,I11,3F11.3,3X,'default category',I3)
END SUBROUTINE OCPLF



!> Prints OC water levels, flows, and channel wetted areas.
SUBROUTINE OCPRI (OCNOW, ARXL, QOC)
!----------------------------------------------------------------------*
!
!  Print results of OC simulation
!
!----------------------------------------------------------------------*
! Entry requirements:
!  NELEE.ge.NEL    NEL.ge.[1,NLF]    NLF.ge.0    NLF.le.size_of_ARXL
!  PRI.ge.0        PRI open for F output
!----------------------------------------------------------------------*
DOUBLEPRECISION, INTENT(IN) :: OCNOW, ARXL(:), QOC(NELEE, 4)
DOUBLEPRECISION             :: ghrf(total_no_links)
INTEGER                     :: FACE, ielmm
!----------------------------------------------------------------------*
WRITE(PPPRI, 9100) 'AFTER', OCNOW, ' HOURS ----'
WRITE(PPPRI, 9200) 'iel', ('QOC(iel,', FACE, ')', FACE = 1, 4) , 'HRF', 'ARXL'
DO ielmm=1,total_no_links
    ghrf(ielmm) = GETHRF(ielmm)
ENDDO
WRITE(PPPRI, 9210) (ielmm, (QOC(ielmm,FACE), FACE=1,4), ghrf(ielmm), ARXL (ielmm), ielmm = 1, total_no_links)
DO 210 ielmm = total_no_links + 1,total_no_elements
  210 WRITE(PPPRI, 9210) ielmm, (QOC (ielmm, FACE), FACE = 1, 4), GETHRF (ielmm)

WRITE(PPPRI, 9100) 'END ----'
 9100 FORMAT (//'---- OC MODULE  RESULTS ',A:F10.2,A//)
 9200 FORMAT (4X,A4,4(2X,A8,I1,A1),2A12/)

 9210 FORMAT (4X,I4,SP,4F12.3,S,2F12.3)
END SUBROUTINE OCPRI



!> Reads and dispatches the static OC input file.
!>
!> `OCREAD` loads timestep/output controls, roughness parameters, initial
!> overland water depths, boundary-condition definitions, and channel-link data.
!> It delegates boundary parsing to [[JEOCBC]] and link geometry to [[OCPLF]].
SUBROUTINE OCREAD(KONT, TDC, TFC, CATR, DDUM2)
!----------------------------------------------------------------------*
!  Control the reading of the OC input data file
!----------------------------------------------------------------------*
! Entry requirements:
!  NELEE.ge.[NEL,NOCTAB*NOCTAB]    NEL.gt.NLF    NLF.ge.0    NOCTAB.ge.1
!  NLFEE.ge.NLF    OCD open for F input      PRI open for F output
!----------------------------------------------------------------------*
INTEGER, INTENT(OUT) :: KONT
DOUBLEPRECISION      :: TDC, TFC
DOUBLEPRECISION      :: CATR (NOCTAB), DDUM2 (NOCTAB, NOCTAB)
INTEGER              :: I, IBC, ICAT, IELt, IXER, KKON, TYPEE
INTEGER              :: NCATR, NLAND, NOCBC, NT, NC (11)
DOUBLEPRECISION      :: DET, SMIN
DOUBLEPRECISION      :: CDRS
LOGICAL              :: BIOWAT, BOUT
CHARACTER(81) :: MSG
CHARACTER(11)  :: CTYPE (11)
ICAT (ielt) = MAX (1, MIN (IDUM (ielt), NCATR) )
DATA NC / 4 * 0, 5, 0, 2 * 4, 2 * 0, 5 /

DATA CTYPE / 'impermeable', '  grid-grid', '       head', ' flux', ' polynomial', ' river_link', '       weir', ' river+weir', &
& '       head', '       flux', ' polynomial' /
!----------------------------------------------------------------------*
!
!               Initialization
!
IXER = 0
NLAND = total_no_elements - total_no_links
NGDBGN = total_no_links + 1
!
!               Integer & logical variables
!:OC1
READ (OCD, * )
READ (OCD, * ) NT, NCATR, KONT, BIOWAT
KKON = MOD (KONT, 2)
BOUT = KKON.EQ.1
IF (BOUT) WRITE(PPPRI, 9080) ' ', NCATR
!
!               OC time-step data
!:OC2
!     (was (PT(I),TEMPS(I),I=1,NT))
READ (OCD, * )
READ (OCD, * )
!
!               Default roughness parameters & floating-point variables
!:OC3
READ (OCD, * )
READ (OCD, * ) SMIN, CDRS, TDC, TFC, DET
IF (KONT.LT.2) TDC = TFC + one
!:OC4
IF (ISZERO(CDRS)) THEN
   IF ((NCATR.GT.NOCTAB).OR.(NCATR.LT.0)) GOTO 8047
   IF (NCATR.GT.0) THEN
      READ (OCD, * ) (CATR (I), I = 1, NCATR)
      IF (BOUT) THEN
         WRITE(PPPRI, 9084) (CATR (I), I = 1, NCATR)
         WRITE(PPPRI, * )
      ENDIF
   ENDIF
ELSEIF (BOUT) THEN
   WRITE(PPPRI, 9082) CDRS
ENDIF
!
!               INITIAL OVERLAND FLOW ELEVATIONS
!:OC5
IF (BIOWAT) THEN
   CALL AREADR (DUMMY, KKON, OCD, PPPRI)
ELSE
   CALL ALINIT (ZERO, NLAND, DUMMY (NGDBGN) )
   IF (BOUT) WRITE(PPPRI, 9085) 'zero'
ENDIF
DO 4 ielt = NGDBGN, total_no_elements
   CALL SETHRF(ielt, ZGRUND (ielt) + DUMMY (ielt))

    4 END DO
!
!               ROUGHNESS PARAMETERS FOR OVERLAND FLOW
!:OC14
!:OC17
IF (NOTZERO(CDRS)) THEN
   CALL ALINIT(CDRS, NLAND, STRXX(NGDBGN) )
   CALL ALINIT(CDRS, NLAND, STRYY(NGDBGN) )
ELSEIF (NCATR.EQ.0) THEN
   CALL AREADR(STRXX, KKON, OCD, PPPRI)
   CALL AREADR(STRYY, KKON, OCD, PPPRI)
ELSE
   CALL AREADI (IDUM(1:nelee), KKON, OCD, PPPRI, NCATR)
   DO 210 ielt = NGDBGN, total_no_elements
  210    STRXX(ielt) = CATR (ICAT (ielt) )
   CALL AREADI (IDUM(1:nelee), KKON, OCD, PPPRI, NCATR)
   DO 220 ielt = NGDBGN,total_no_elements
  220    STRYY(ielt) = CATR (ICAT (ielt) )

ENDIF
!               BOUNDARY CONDITIONS
!
CALL JEOCBC(IXER, NOCBC)
!
!               PARAMETERS OF RIVER LINKS
!
IF ((total_no_links.GT.0).AND.(IXER.EQ.0)) THEN
    CALL OCPLF(BOUT, IXER, NOCBCD(:, 2:4), IDUM(1:noctab), DDUM2)
ENDIF
!
!               FINISH
!
REWIND(OCD) !CLOSE (OCD)    !AD
IF (IXER.NE.0) THEN
   WRITE (MSG, 9412) IXER
   CALL ERROR(FFFATAL, 1049, PPPRI, 0, 0, MSG)
ELSEIF (BOUT) THEN
   WRITE(PPPRI, 9500) 'no-flow'
   IF (NOCBC.GT.0) WRITE(PPPRI, 9600) 'Index', 'Element', 'Face', &
    'Type', 'Category', 'Coefficients'
   DO 2000 IBC = 1, NOCBC
      TYPEE = NOCBCD (IBC, 3)
      WRITE(PPPRI, 9610) IBC, (NOCBCD (IBC, I), I = 1, 2), CTYPE ( &
       TYPEE), NOCBCD (IBC, 4), (COCBCD (I, IBC), I = 1, NC (TYPEE) )
 2000    END DO
   WRITE(PPPRI, 9080) ' END OF '
ENDIF

RETURN
 8047 WRITE (MSG, 9004) NCATR, NOCTAB

CALL ERROR(FFFATAL, 1047, PPPRI, 0, 0, MSG)

 9004 FORMAT('Number of roughness categories NCATR =',I4,2X, &
&       'lies outside range 0:NOCTAB = 0 :',I4)

 9080 FORMAT (///'---- OC MODULE ',A,'INPUT DATA PROCESSING ----'///: &
&          5X,'NUMBER OF DIFFERENT OVERLAND FLOW ROUGHNESS', &
&             ' CATEGORIES   NCATR = ',I4 )

 9082 FORMAT (/5X,'DEFAULT VALUE OF OVERLAND FLOW ROUGHNESS ', &
&             'COEFFICIENT     CDRS = ', F8.2)

 9084 FORMAT (/4X,' ROUGHNESS COEFFICIENTS  CATR  ATTACHED TO', &
&            ' EACH OF THE NCATR CATEGORIES' / (10F10.2))

 9085 FORMAT (/5X,'Initial overland water depth is ',A)

 9412 FORMAT (I5,' ERROR(S) FOUND DURING OC INPUT DATA PROCESSING')

 9500 FORMAT (/5X,'Default OC B.C. is ',A,' at catchment boundaries ', &
&            'and at channel/bank dead-ends')

 9600 FORMAT (/5X,'OC Boundary Conditions:'//5X,3A8,A12,A10,A14)

 9610 FORMAT (5X,3I8,A12,I10,1P,5G14.6)
END SUBROUTINE OCREAD



!> Advances the overland/channel flow solution by one OC timestep.
!>
!> `OCSIM` reads current boundary values, calls [[OCQDQ]] for nonlinear flow and
!> derivative terms, assembles the row-wise implicit matrix with [[OCABC]], solves
!> the block tridiagonal system by forward row elimination and backward
!> substitution, updates water levels and inter-element flows, applies [[OCFIX]]
!> to remove spurious negative internal flows, computes channel wetted area, and
!> optionally prints OC diagnostics.
!>
!> The routine uses frame topology and geometry (`ICMREF`, `ICMRF2`, `NOCBCC`,
!> `NOCBCD`, `AREA`, `CLENTH`, `DXQQ`, `DYQQ`, `DHF`, `ZGRUND`, `CWIDTH`,
!> `ZBFULL`), forcing terms (`PNETTO`, `ESWA`, `QH`, `QOCF`, `HOCNOW`), OC
!> row indices (`NROWF`, `NROWL`, `NROWST`, `NROWEL`, `NELIND`), cross-section
!> and roughness tables (`NXSECT`, `XINH`, `XINW`, `XAREA`, `XSTAB`, `STRX`,
!> `STRY`), and timing controls (`OCNOW`, `OCNEXT`, `TDC`, `TFC`). It updates
!> `HRF` and writes `QSA`, `QOC`, `DQ0ST`, `DQIST`, `DQIST2`, and `ARXL`.
!>
!> The OC timestep is converted to seconds as
!>
!> \[
!> DTOC = 3600\,OCNEXT.
!> \]
!>
!> After [[OCQDQ]] has evaluated current flows and derivatives, [[OCABC]]
!> assembles one block row for each y-row. For row \(r\), with water-level
!> correction vector \(d_r\), the assembled equation has the block-tridiagonal
!> form
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
!> The solved correction for each element is applied directly to water level,
!>
!> \[
!> HRF_e^{n+1}=HRF_e^n+d_e.
!> \]
!>
!> Face flows are advanced with the same first-order linearisation used in the
!> matrix assembly:
!>
!> \[
!> Q_{e,f}^{n+1}=Q_{e,f}^n + DQ0ST_{e,f}d_e
!> + \sum_j DQIST_{e,f,j}d_j,
!> \]
!>
!> where a single neighbour uses `DQIST` and a multi-link junction expands the
!> neighbour sum through `ICMRF2` and `DQIST2`. [[OCFIX]] is then called to
!> remove spurious negative internal flows and adjust the corresponding water
!> levels.
!>
!> `QOC` is copied from the internal face-flow array and converted from the
!> OC face convention to the model x/y convention by changing the sign on faces
!> 1 and 2. For each channel link, wetted area is interpolated from the
!> cross-section table. If \(H=HRF-ZGRUND\) falls between tabulated depths
!> \(H_m\) and \(H_i\),
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
!> Above the last table level, the link is extended with rectangular bankfull
!> width:
!>
!> \[
!> ARXL = XAREA_N + (HRF-ZBFULL)\,CWIDTH.
!> \]
!>
!> If `QMAX > 0`, all channel-link face flows are checked against this maximum
!> and a fatal diagnostic is issued when it is exceeded.
SUBROUTINE OCSIM

INTEGER         :: I, IELs, IND, IROW, IBC, IBR, ICOD, IFACE, IHB, IM, IRSV
INTEGER         :: J, JEL, JND, JROW, K0, LINK, N, NCR, NPR, NSV  , face
!INTEGER         :: LCdum (NXOCEE, 2)
!INTEGER         :: ijedum(nelee,4,2:3), ijedum2(nlfee,3,2), kk, ll, vv  !afromICMREF (NELEE, 4, 2:3), afromICMRF2 (NLFEE, 3, 2)
INTEGER         :: kk, ll, vv
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ijedum, ijedum2
DOUBLEPRECISION :: DDI, DH, DQ, DW, H, HI, HM, OCTIME, WI, WM, Z
!DOUBLEPRECISION :: AA (NXOCEE, NXOCEE), DD (NXOCEE, NYEE), FF(NXOCEE)
!DOUBLEPRECISION :: BB (NXOCEE, NXOCEE), GG (NXOCEE, NYEE)
!DOUBLEPRECISION :: CC (NXOCEE, NXOCEE), EE (NXOCEE, NXOCEE, NYEE)
!DOUBLEPRECISION :: TM1 (NXOCEE, NXOCEE), TV1 (NXOCEE) !, FWRK (NXSCEE * 12)
!DOUBLEPRECISION :: TM2 (NXOCEE, NXOCEE), TV2 (NXOCEE)
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: AA, DD
DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: FF
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: BB, GG
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: CC
DOUBLEPRECISION, DIMENSION(:,:,:), ALLOCATABLE :: EE
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: TM1, TM2
DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: TV1,TV2


!DOUBLEPRECISION, DIMENSION(total_no_elements)    :: inhrf
!DOUBLEPRECISION, DIMENSION(total_no_elements)    :: GGGETHRF
!DOUBLEPRECISION, DIMENSION(total_no_elements,4)  :: inqsa
!DOUBLEPRECISION, DIMENSION(total_no_elements,4)  :: GGGETQSA
DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE      :: inhrf
DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE      :: GGGETHRF
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE    :: inqsa
DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE   :: GGGETQSA



LOGICAL         :: g8018, g8029, cycle255
LOGICAL         :: first=.TRUE.
CHARACTER(36)   :: MSG
!----------------------------------------------------------------------*
!
! ----- Initialize
! needs to be nelee and nlfee here as it reads from arrays that are still set to these sizes
allocate   (ijedum(nelee,4,2:3),ijedum2(nlfee,3,2))
ijedum=0
ijeduM2=0


allocate   (AA(NX*4,NX*4),DD(NX*4,NY))
allocate   (FF(NX*4))
allocate   (BB(NX*4,NX*4),GG(NX*4,NY))
allocate   (CC(NX*4,NX*4))
allocate   (EE(NX*4,NX*4,NY))
allocate   (TM1(NX*4,NX*4),TM2(NX*4,NX*4))
allocate   (TV1(NX*4),TV2(NX*4))
AA=0.0d0
DD=0.0d0
FF=0.0d0
BB=0.0d0
GG=0.0d0
CC=0.0d0
EE=0.0d0
TM1=0.0d0
TM2=0.0d0
TV1=0.0d0
TV2=0.0d0

allocate   (inhrf(total_no_elements))
allocate   (GGGETHRF(total_no_elements))
allocate   (inqsa(total_no_elements,4))
allocate   (GGGETQSA(total_no_elements,4))
inhrf=0.0d0
GGGETHRF=0.0d0
inqsa=0.0d0
GGGETQSA=0.0d0


!
DTOC = OCNEXT * 3600.D0
IF (FIRST) THEN
    first = .FALSE.
    DO LINK = 1, total_no_links
        XAFULL (LINK) = XAREA (LINK, NXSECT (LINK) )
    ENDDO
ENDIF
!
! ----- GET PRESCRIBED BOUNDARY VALUES HOCNOW & QOCF
! sb 120514 put this line back in
!
CALL OCEXT
!
! ----- CALCULATE FLOWS QSA & DERIVATIVES DQ0ST,DQIST,DQIST2
!

CALL OCQDQ ()
!
! ----- LOOP OVER ROWS, CALCULATING EE & GG
!
NCR = 0
g8018=.FALSE.
g8029=.FALSE.
out44 : DO IROW = NROWF, NROWL
    IF(g8018) CYCLE out44
    IRSV = IROW + 1
    !
    !        NCR    : NUMBER OF ELEMENTS IN THE CURRENT ROW
    !        NPR    : NUMBER OF ELEMENTS IN THE PREVIOUS ROW
    !        NSV    : NUMBER OF ELEMENTS IN THE NEXT (SUIVANT) ROW
    !
    NPR = NCR
    K0 = NROWST (IROW) - 1
    NCR = NROWST (IRSV) - 1 - K0
    IF (NCR.EQ.0) CYCLE out44 !GOTO 44
    NSV = NROWST (MIN (IRSV, NROWL) + 1) - NROWST (IRSV)
    !
    ! CALCULATE MATRICES AA, BB, CC, FF
    !
    DO IND = 1, NCR
        iels = NROWEL (IND+K0)
        LINK = MAX (1, MIN (iels, total_no_links) )
        IBC = NOCBCC (iels)
        IF (IBC.GT.0) THEN
            IHB = NOCBCD (IBC, 4)
            IBC = NOCBCD (IBC, 3)
        ELSE
            IHB = 1
        ENDIF
        CALL OCABC (IND, IROW, iels, NSV, NCR, NPR, IBC, NXSECT ( &
                    LINK), cellarea (iels), ZGRUND (iels), CLENTH (LINK), ZBFULL ( &
                    LINK), GETHRF (iels), PNETTO (iels), QH (iels), ESWA (iels), &
                    HOCNOW (IHB), AA(:,IND), BB (1:ncr,IND), CC(:,IND), &
                    FF (IND) )
    ENDDO
    !
    ! CALCULATE MATRIX TM2 (inverse of CC.EE+BB) AND VECTOR TV2 (FF-CC.GG)
    !
    IF (IROW.EQ.NROWF) THEN
        DO IND = 1, NCR
            TM2(1:ncr,IND) = BB(1:ncr,IND)
            !CALL DCOPY (NCR, BB(1:ncr,IND), 1, TM2(1:ncr,IND), 1)
        ENDDO
         TV2(1:ncr) = FF(1:ncr)
        !CALL DCOPY(NCR, FF(1:ncr), 1, TV2(1:ncr), 1)
    ELSE
        !CALL MULMM (TM1, CC, EE (1, 1, IROW), NCR, NPR, NCR, NXOCEE)
        tm1(1:ncr,1:ncr) = JEMATMUL_MM(cc(1:npr,1:ncr), ee(1:ncr,1:npr,irow), ncr, npr, ncr)

        !CALL ADDMM (TM2, BB, TM1, NCR, NCR, NXOCEE)
        tm2(1:ncr,1:ncr) = bb(1:ncr,1:ncr) + tm1(1:ncr,1:ncr)

        !CALL MULMV (TV1, CC, GG (1, IROW), NCR, NPR, NXOCEE)
        tv1(1:ncr) = JEMATMUL_VM(cc(1:npr,1:ncr), gg(1:npr,irow), ncr, npr)

        !CALL DIFVV (TV2, FF, TV1, NCR, NXOCEE)
         TV2(1:ncr) = FF(1:ncr) - TV1(1:ncr)
    ENDIF
    !CALL PMINVM (TM2(1:ncr,1:ncr), NCR, ICOD)
    CALL INVERTMAT(TM2(1:ncr,1:ncr), NCR, ICOD)
    IF (ICOD.EQ.1) THEN
        g8018=.TRUE.
        CYCLE out44
    ENDIF
    !
    ! CALCULATE MATRIX EE(IROW+1)
    !
    IF (IROW.NE.NROWL) THEN
        !CALL MULMM (EE (1, 1, IRSV), TM2, AA, NCR, NCR, NSV, NXOCEE)
        ee(1:nsv,1:ncr,irsv) = JEMATMUL_MM(tm2(1:ncr,1:ncr), aa(1:nsv,1:ncr), ncr, ncr, nsv)

        !CALL CHSGN (EE (1, 1, IRSV), NCR, NSV, NXOCEE)
        ee(1:nsv,1:ncr,irsv) = - ee(1:nsv,1:ncr,irsv)
    ENDIF
    !
    ! CALCULATE VECTOR GG(IROW+1)
    !
    !CALL MULMV (GG (1, IRSV), TM2, TV2, NCR, NCR, NXOCEE)
    gg(1:ncr,irsv) = JEMATMUL_VM(tm2(1:ncr,1:ncr), tv2(1:ncr), ncr, ncr)
    !
ENDDO out44
IF(g8018) THEN
    !AD WRITE (MSG, '(A,I4)') 'Singular matrix at row', IROW
    CALL ERROR(FFFATAL, 1018, PPPRI, NROWEL (NROWST (IROW) ), 0, MSG)
ELSE
    !
    ! ----- DOWNWARDS SWEEP, CALCULATION OF DD
    !
    !     * last row first (use NCR,IRSV from loop above)
    IROW = NROWL
    DD(1:ncr,IROW) = GG(1:ncr,IRSV)
    !ALL DCOPY(NCR, GG(1:ncr,IRSV), 1, DD(1:ncr,IROW), 1)
    !     * loop over remaining rows
    DO IROW = NROWL - 1, NROWF, - 1
        IRSV = IROW + 1
        NSV = NCR
        NCR = NROWST (IRSV) - NROWST (IROW)
        !CALL MULMV (TV1, EE (1, 1, IRSV), DD (1, IRSV), NCR, NSV, NXOCEE)
        tv1(1:ncr) = JEMATMUL_VM(ee(1:nsv,1:ncr,irsv), dd(1:nsv,irsv), ncr, nsv)

        !CALL ADDVV (DD (1, IROW), TV1, GG (1, IRSV), NCR)
        dd(1:ncr,irow) = tv1(1:ncr) + gg(1:ncr,irsv)
    ENDDO
    !
    ! ----- ADVANCE WATER LEVELS AND FLOWS TO TIME LEVEL N+1,
    !       USING FIRST ORDER DERIVATIVES OF FLOWS AT TIME LEVEL N
    !
    DO iels = 1,total_no_elements
        IND = NELIND (iels)
        IROW = ICMREF (iels, 3)
        DDI = DD (IND, IROW)
        CALL SETHRF(iels, GETHRF (iels) + DDI)
        DO IFACE = 1, 4
            DQ = DQ0ST (iels, IFACE) * DDI
            JEL = ICMREF (iels, IFACE+4)
            IF (JEL.GT.0) THEN
                JND = NELIND (JEL)
                JROW = ICMREF (JEL, 3)
                DQ = DQIST (iels, IFACE) * DD (JND, JROW) + DQ
            ELSEIF (JEL.LT.0) THEN
                IBR = - JEL
                DO J = 1, 3
                    JEL = ICMRF2 (IBR, J)
                    IF (JEL.GT.0) THEN
                        JND = NELIND (JEL)
                        JROW = ICMREF (JEL, 3)
                        DQ = DQIST2 (IBR, J) * DD (JND, JROW) + DQ
                    ENDIF
                ENDDO
            ENDIF
            CALL SETQSA(iels, IFACE, GETQSA(iels, IFACE) + DQ)
        ENDDO
    ENDDO
    !
    ! CHECK FOR SPURIOUS NEGATIVE FLOWS, AND RECALCULATE WATER LEVELS
    ! IF REQUIRED.  NB. DOES NOT CHECK BOUNDARY FLOWS
    !
    ! called routine has afromICMREF (NELEE, 4, 2:3), afromICMRF2 (NLFEE, 3, 2)
    vv=5
    DO LL=2,3       !AD PASSING PROBLEMS (use local dummies)
        DO kk=1,4   !array order
            ijedum(:,kk,LL) = icmref(:,vv)
            vv = vv + 1
        ENDDO
    ENDDO
    vv = 1
    DO LL=1,2
        DO kk=1,3  !array order
            ijedum2(:,kk,LL) = icmrf2(:,vv)
            vv = vv + 1
        ENDDO
    ENDDO
    !CALL OCFIX (ICMREF (1, 5), ICMRF2, AREA, DTOC, QSA)
    !CALL OCFIX(ijedum, ijedum2, total_no_elements, dtoc)  !AD PASSING PROBLEMS (use local dummies)

    !ad this untidy mess is for debugging of tangent
    DO vv=1,total_no_elements
        inhrf(vv) = GETHRF(vv)
        DO face=1,4
            inqsa(vv,face) = GETQSA(vv,face)
        ENDDO
    ENDDO


CALL OCFIX(ijedum, ijedum2, total_no_elements, dtoc, inhrf, GGGETHRF, inqsa, GGGETQSA)  !AD PASSING PROBLEMS (use local dummies)!




    DO vv=1,total_no_elements
        CALL SETHRF(vv, GGGETHRF(vv))
        DO face=1,4
            CALL SETQSA(vv, face, GGGETQSA(vv,face))
        ENDDO
    ENDDO




    ! SET FLOWS QOC (POSITIVE X,Y) FOR USE BY OTHER COMPONENTS
    !
    !DO IFACE = 1, 4
        QOC(1:total_no_elements,:) = GETQSA_ALL(total_no_elements)
        !CALL DCOPY(NEL, QSA(1:nel,IFACE), 1, QOC(1:nel,IFACE), 1)
    !ENDDO
    !CALL CHSGN (QOC, 2, NEL, NELEE)
    qoc(1:total_no_elements,1:2) = - qoc(1:total_no_elements,1:2)
    !
    ! ----- CALCULATE CROSS-SECTIONAL AREA OF CHANNEL WATER
    !
    out255 : DO iels = 1, total_no_links
        cycle255=.FALSE.
        Z = GETHRF (iels)
        H = Z - ZGRUND (iels)
        N = NXSECT (iels)
        out250 : DO I = 2, N
            IF(cycle255) CYCLE out250
            HI = XINH (iels, I)
            IF (H.LT.HI) THEN
                IM = I - 1
                HM = XINH (iels, IM)
                WM = XINW (iels, IM)
                WI = XINW (iels, I)
                DH = H - HM
                DW = (WI - WM) * (DH / (HI - HM) )
                ARXL (iels) = XAREA (iels, IM) + (WM + half * DW) * DH
                cycle255 = .TRUE.
            ENDIF
        ENDDO out250
        IF(cycle255) CYCLE out255
        ARXL (iels) = XAREA (iels, N) + (Z - ZBFULL (iels) ) * CWIDTH ( iels)
    ENDDO out255
    !
    ! ----- Print results
    !
    OCTIME = OCNOW + OCNEXT
    IF ((OCTIME.GE.TDC).AND.(OCTIME.LE.TFC)) CALL OCPRI (OCTIME, ARXL, QOC)
    !
    ! ----- CHECK FOR CHANNEL BLOW-UP
    !
    IF (GTZERO(QMAX)) THEN
        out270 : DO iels = 1, total_no_links
            IF(g8029) CYCLE out270
            out260 : DO IFACE = 1, 4
                IF(g8029) CYCLE out260
                IF (ABS (QOC (iels, IFACE) ) .GT.QMAX) g8029 = .TRUE.
            ENDDO out260
        ENDDO out270
    ENDIF
ENDIF
IF(g8029) THEN
     MSG = 'CHANNEL FLOWS EXCEED MAXIMUM ALLOWED'
    CALL ERROR(FFFATAL, 1029, PPPRI, iels, 0, MSG)
ENDIF
END SUBROUTINE OCSIM



!> Builds channel cross-section area and conveyance lookup tables.
!>
!> `OCXS` integrates tabulated width-depth pairs to water area, derives an
!> effective bed elevation for full-bank storage, and fills `XSTAB` with
!> uniformly spaced depth, conveyance, and conveyance-derivative values used by
!> the OC flow calculation.
!>
!> The SHETRAN User Guide and Data Input Manual defines the channel
!> cross-section data in records `OC30`-`OC34`: each cross-section category is
!> supplied as width/depth pairs (`XDEFW`, `XDEFH`), the first depth must be
!> zero, and the final depth defines the bankfull depth. The manual also states
!> that channel flow uses the user-supplied cross-section, while subsurface-flow
!> exchange uses an effective rectangular channel with the same cross-sectional
!> area.
!>
!> In the code these manual fields are stored per link as `XINW(link,j)` and
!> `XINH(link,j)`, with `NXSECT(link)` width/depth pairs and roughness
!> `STRXX(link)`. Entry requirements retained from the legacy routine are:
!> at least one active link, `NXSCEE >= 2`, positive channel widths
!> `CWIDTH(1:total_no_links)`, `NXSECT` values within the allocated
!> `XINH`/`XINW`/`XAREA` table sizes, a positive final tabulated depth, and
!> strictly increasing tabulated depths within each link.
!>
!> Input width-depth pairs are integrated by the trapezoidal rule. For tabulated
!> level \(j\),
!>
!> \[
!> XAREA_j = XAREA_{j-1}
!> + \frac{1}{2}\left(XINW_j+XINW_{j-1}\right)
!>   \left(XINH_j-XINH_{j-1}\right),
!> \]
!>
!> with `XAREA(:,1)=0`. The manual's effective rectangular-channel statement is
!> implemented by shifting the effective bed elevation so that a rectangle of
!> width `CWIDTH` has the same bankfull area as the tabulated cross-section:
!>
!> \[
!> ZBEFF = ZBFULL - XAREA_N/CWIDTH.
!> \]
!>
!> The lookup table `XSTAB` supports the OC flow calculation without repeatedly
!> integrating the irregular cross-section. It has rows:
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
!> \(H_i \le h_j \le H_{i+1}\) is found. Width is treated as linearly varying
!> between the two tabulated width/depth points:
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
!> `CONVEYAN` converts \(A_j\), depth, and roughness `STRXX` into conveyance.
!> For `OCXS` it is called with `ty=0`, so the main branch used away from
!> near-zero depth is the Gauckler-Manning-Strickler-style relation implemented
!> in [[ocmod2:conveyan]]:
!>
!> \[
!> C_j = STRXX\,A_j\,h_j^{2/3}.
!> \]
!>
!> For \(10^{-9} \le h_j < 10^{-3}\) m, the code uses the smoothed polynomial
!> branch in `CONVEYAN` for automatic-differentiation stability:
!>
!> \[
!> C_j = STRXX\,A_j\,\frac{10}{3}\,h_j(4-1000h_j),
!> \]
!>
!> and for smaller depths it returns zero conveyance. The stored derivative in
!> `XSTAB` is not the derivative returned by `CONVEYAN`; `OCXS` stores the
!> finite-difference slope for interval `j-1`:
!>
!> \[
!> XSTAB_{3,j-1} = \frac{C_j-C_{j-1}}{\Delta h},
!> \]
!>
!> so `XSTAB(2,j) + XSTAB(3,j)*(h-XSTAB(1,j))` is continuous and piecewise
!> linear in water depth. As in the legacy routine, conveyance and derivative
!> entries for the final lookup row are not defined.
SUBROUTINE OCXS ()
INTEGER         :: I, IELr, J, N
DOUBLEPRECISION :: ALPHA, DH, HI, HIP1, HJ, STEPH, STR, W2, XAJ, XCJ, XCJM1, adumy
!----------------------------------------------------------------------*

DO 500 ielr = 1, total_no_links
!
! LOCAL VARIABLES
!
   N = NXSECT (ielr)

   STR = STRXX(ielr)
!
! SET UP CROSS-SECTIONAL AREAS FOR EACH OF THE INPUT LEVELS
!
   XAREA (ielr, 1) = zero
   DO 180 J = 2, N
      W2 = XINW (ielr, J) + XINW (ielr, J - 1)
      DH = XINH (ielr, J) - XINH (ielr, J - 1)
      XAREA (ielr, J) = XAREA (ielr, J - 1) + W2 * DH * half

  180    END DO
!
! EFFECTIVE BED ELEVATION
!

   ZBEFF (ielr) = ZBFULL (ielr) - XAREA (ielr, N) / CWIDTH (ielr)
!
! SET UP FULL CROSS-SECTION TABLES OF HEIGHT, CONVEYANCE & DERIVATIVE
!
! NOTE: The formulation is such that
!             XSTAB(2,j,ielr) + XSTAB(3,j,ielr)*( h - XSTAB(1,j,ielr) )
!       is a continuous (piecewise linear) function of h
!
   I = 1
   HI = XINH (ielr, I)
   STEPH = XINH (ielr, N) / (NXSCEE-1)
   XCJ = zero
   XSTAB (1, 1, ielr) = zero
   DO 200 J = 2, NXSCEE
      XCJM1 = XCJ
      HJ = STEPH * (J - 1)


  !orig
  !190       HIP1  = XINH(ielr,I+1)
  !          IF ( I.LT.N-1 .AND. HIP1.LT.HJ ) THEN
  !               I = I + 1
  !              HI = HIP1
  !              GOTO 190
  !          ENDIF


        DO
            HIP1  = XINH(ielr,I+1)
            IF (.NOT.( (I.LT.N-1) .AND. (HIP1.LT.HJ) )) EXIT
                 I = I + 1
                HI = HIP1
        ENDDO





  !    iscycle=.FALSE.  !NOT QUITE RIGHT
  !    DO I=I,N-1
  !      IF(iscycle) CYCLE
  !      HIP1 = XINH (ielr,I+1)
  !      IF (HIP1<HJ) THEN
  !           HI = HIP1
  !       ELSE
  !         iscycle=.true.
  !      ENDIF
  !    ENDDO


      DH = HJ - HI
      ALPHA = DH / (HIP1 - HI)
      W2 = (2D0 - ALPHA) * XINW (ielr, I) + ALPHA * XINW (ielr, I + &
       1)
      XAJ = XAREA (ielr, I) + W2 * DH * half

      !XCJ = STR * XAJ * HJ**F23
      CALL CONVEYAN(str, hj, xcj, adumy, 0, xaj)
      XSTAB (1, J, ielr) = HJ
      XSTAB (2, J - 1, ielr) = XCJM1
      XSTAB (3, J - 1, ielr) = (XCJ - XCJM1) / STEPH

  200    END DO

  500 END DO
END SUBROUTINE OCXS



!> Returns the channel link number at a grid coordinate and orientation.
!>
!> `LINKNO` searches the link reference table for a north-south or east-west
!> link whose stored grid coordinate matches the requested `(I,J)` location.
INTEGER FUNCTION LINKNO (I, J, NSOUTH)
! GET LINK NUMBER, GIVEN X, Y CO-ORDINATES AND ORIENTATION
LOGICAL, INTENT(IN) :: NSOUTH
INTEGER, INTENT(IN) :: i, j
INTEGER             :: L
LOGICAL             :: jedum, iscycle !NEEDED FOR AD
LINKNO = 0
IF (total_no_links.EQ.0) RETURN
iscycle=.FALSE.
DO L = 1, total_no_links
    IF(iscycle) CYCLE
    jedum = NSOUTH.EQV.LINKNS (L)
   IF ((ICMREF (L, 2) .EQ.I) .AND. (ICMREF (L, 3) .EQ.J) .AND. jedum ) THEN
      LINKNO = L
      iscycle=.TRUE.
   ENDIF
ENDDO
END FUNCTION LINKNO
END MODULE OCmod
