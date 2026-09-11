!> summary: The `OC01`--`OC22` overland and channel input records.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[OCREAD]] reads the overland/channel data groups: the Strickler roughness
!> grids, the face topology codes, the cross-section categories and the
!> boundary-condition records. [[JEOCBC]] and [[OCPLF]] are its helpers for
!> the boundary records and the link/face plan.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of OCmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_input

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, one, zero
   USE array_limits, ONLY: nelee, NOCTAB, nxee, nyee
   USE element_geometry, ONLY: NBFACE, total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF, ICMXY, NGDBGN, NX, NY
   USE channel_geometry, ONLY: CWIDTH, ZBFULL
   USE file_units, ONLY: FID_logfile, OCD
   USE oc_state, ONLY: LCODEX, LCODEY, STRXX, STRYY
   USE oc_boundaries, ONLY: COCBCD, NOCBCC, NOCBCD, NOCFB, NOCHB
   USE oc_cross_sections, ONLY: NXSECT, XINH, XINW
   USE oc_indexing, ONLY: LINKNO
   USE oc_node_solver, ONLY: sethrf
   USE grid_arrays, ONLY: AREADI, AREADR
   USE float_compare, ONLY: iszero, notzero
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal, ERRLVL_error
   USE error_status, ONLY: errstat_read, errstat_rewind

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: OCREAD

CONTAINS

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
!> by [[frame_geometry:FRIND]].
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
      INTEGER                      :: KFACE, NOCPB, TYPEE, ios
      DOUBLE PRECISION             :: ADUM(5)
      LOGICAL                      :: TEST
      CHARACTER(LEN=77)            :: MSG
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'oc_input:JEOCBC' !! Location string for read-error reports.
      INTEGER, DIMENSION(NXEE*NYEE), SAVE :: IDUM !! Integer input workspace; scratch within this routine only. `SAVE` keeps it in static storage, as the former module variable was.

      !----------------------------------------------------------------------*

      ! NUMBER OF CATEGORIES FOR EACH TYPE
      READ (OCD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (OCD, *, IOSTAT=ios, IOMSG=emsg) NOCHB, NOCFB, NOCPB
      CALL errstat_read(ios, location, emsg)

      ! INITIALIZATION
      NOCBC = 0

      ! Vectorized zeroing for large array
      NOCBCC(1:total_no_elements) = 0

      ! HEAD BOUNDARY (TYPE 3)
      IF (NOCHB > 0) THEN
         MSG = 'ERROR IN OC HEAD BOUNDARY GRID'
         CALL AREADI(IDUM, 0, OCD, FID_logfile, NOCHB)

         DO IELY = NGDBGN, total_no_elements
            ICAT = IDUM(IELY)
            IF (ICAT < 0 .OR. ICAT > NOCHB) THEN
               IXER = IXER + 1
               CALL RAISE_ERROR(ERRLVL_error, 1020, FID_logfile, IELY, 0, MSG)
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
         CALL AREADI(IDUM, 0, OCD, FID_logfile, NOCFB)

         DO IELY = NGDBGN, total_no_elements
            ICAT = IDUM(IELY)
            IF (ICAT < 0 .OR. ICAT > NOCFB) THEN
               IXER = IXER + 1
               CALL RAISE_ERROR(ERRLVL_error, 1021, FID_logfile, IELY, 0, MSG)
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
         CALL AREADI(IDUM, 0, OCD, FID_logfile, NOCPB)

         DO IELY = NGDBGN, total_no_elements
            ICAT = IDUM(IELY)
            IF (ICAT < 0 .OR. ICAT > NOCPB) THEN
               IXER = IXER + 1
               CALL RAISE_ERROR(ERRLVL_error, 1022, FID_logfile, IELY, 0, MSG)
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
         READ (OCD, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)

         DO I = 1, NOCPB
            READ (OCD, *, IOSTAT=ios, IOMSG=emsg) ICAT, ADUM
            CALL errstat_read(ios, location, emsg)
            IF (ICAT /= I) THEN
               IXER = IXER + 1
               CALL RAISE_ERROR(ERRLVL_error, 1031, FID_logfile, IELY, 0, MSG)
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
         CALL RAISE_ERROR(ERRLVL_error, 1050, FID_logfile, 0, 0, MSG)
      END IF

      DO IBC = 1, MIN(NOCBC, NOCTAB)
         IELY = NOCBCD(IBC, 1)
         JBC = NOCBCC(IELY)
         IF (JBC /= IBC) THEN
            IXER = IXER + 1
            WRITE (MSG, "('Element has multiple OC boundary conditions (types',I2,' and',I2,')')") NOCBCD(IBC, 3), NOCBCD(JBC, 3)
            CALL RAISE_ERROR(ERRLVL_error, 1059, FID_logfile, IELY, 0, MSG)
         END IF
      END DO

   END SUBROUTINE JEOCBC

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
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'oc_input:OCPLF' !! Location string for read-error reports.

      !----------------------------------------------------------------------*
      !
      ! READ DEFAULT CHANNEL CROSS-SECTIONS
      ! :OC30

      READ (OCD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (OCD, *, IOSTAT=ios, IOMSG=emsg) NDEFCT
      CALL errstat_read(ios, location, emsg)

      IF ((NDEFCT > NOCTAB) .OR. (NDEFCT < 0)) THEN
         WRITE (MSG, 9054) NDEFCT, NOCTAB
         CALL RAISE_ERROR(ERRLVL_error, 1054, FID_logfile, 0, 0, MSG)
         IXER = IXER + 1
      END IF

      g8013 = .FALSE.
      g8055 = .FALSE.
      g8300 = .FALSE.
      greturn = .FALSE.

      ! :OC32
      IF (NDEFCT > 0) THEN
         READ (OCD, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
         IF (BOUT) WRITE (FID_logfile, 9032) 'Category', 'Width', 'Height'

         out100: DO IDEF = 1, NDEFCT
            IF (g8055) CYCLE out100
            READ (OCD, *, IOSTAT=ios, IOMSG=emsg) N
            CALL errstat_read(ios, location, emsg)

            IF ((N > NOCTAB) .OR. (N < 2)) THEN
               g8055 = .TRUE.
               CYCLE out100
            END IF

            NXDEF(IDEF) = N
            READ (OCD, *, IOSTAT=ios, IOMSG=emsg) (XDEFW(IDEF, J), XDEFH(IDEF, J), J=1, N)
            CALL errstat_read(ios, location, emsg)

            IF (BOUT) WRITE (FID_logfile, 9034) IDEF, (XDEFW(IDEF, J), XDEFH(IDEF, J), J=1, N)
         END DO out100
      END IF

      !
      ! READ DATA FOR EACH LINK
      ! :OC35
      IF (g8055) THEN
         WRITE (MSG, 9055) IDEF, N, NOCTAB
         CALL RAISE_ERROR(ERRLVL_error, 1055, FID_logfile, 0, 0, MSG)
         IXER = IXER + 1
      ELSE
         READ (OCD, *, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_read(ios, location, emsg)
         IF (BOUT) WRITE (FID_logfile, 9035) 'Element', 'Elevation', 'Init.Depth', 'Strickler', 'Width', 'Height'

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
               CALL RAISE_ERROR(ERRLVL_error, 1012, FID_logfile, ielm, 0, MSG)
               IXER = IXER + 1

               IF (TEST) THEN
                  greturn = .TRUE.
                  CYCLE out500
               END IF

            ELSE
               IF (IDEFX > 0) THEN
                  N = IDEFX
                  READ (OCD, *, IOSTAT=ios, IOMSG=emsg) (XINW(ielm, J), XINH(ielm, J), J=1, N)
                  CALL errstat_read(ios, location, emsg)
                  IF (BOUT) WRITE (FID_logfile, 9037) ielm, ZG, WDEPTH, STR, (XINW(ielm, J), XINH(ielm, J), J=1, N)
               ELSE
                  IDEF = -IDEFX
                  N = NXDEF(IDEF)
                  ! Native Fortran array slice copying N elements
                  XINH(ielm, 1:N) = XDEFH(IDEF, 1:N)
                  XINW(ielm, 1:N) = XDEFW(IDEF, 1:N)
                  IF (BOUT) WRITE (FID_logfile, 9137) ielm, ZG, WDEPTH, STR, IDEF
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
                  READ (OCD, *, IOSTAT=ios, IOMSG=emsg) fromNOCBCD(IBC, 2), (COCBCD(J, IBC), J=1, 4)
                  CALL errstat_read(ios, location, emsg)
                  fromNOCBCD(IBC, 4) = 1
               ELSE IF (TYPEE == 9) THEN
                  fromNOCBCD(IBC, 2) = 0
                  READ (OCD, *, IOSTAT=ios, IOMSG=emsg) fromNOCBCD(IBC, 4)
                  CALL errstat_read(ios, location, emsg)
               ELSE IF (TYPEE == 10) THEN
                  READ (OCD, *, IOSTAT=ios, IOMSG=emsg) (fromNOCBCD(IBC, J), J=2, 4, 2)
                  CALL errstat_read(ios, location, emsg)
               ELSE IF (TYPEE == 11) THEN
                  READ (OCD, *, IOSTAT=ios, IOMSG=emsg) fromNOCBCD(IBC, 2), (COCBCD(J, IBC), J=1, 5)
                  CALL errstat_read(ios, location, emsg)
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
         CALL RAISE_ERROR(ERRLVL_error, 1013, FID_logfile, ielm, 0, MSG)
         IXER = IXER + 1
      ELSE IF (g8300) THEN
         MSG = 'Channel input data is missing or has incorrect format'
         CALL RAISE_ERROR(ERRLVL_error, 1019, FID_logfile, ielm, 0, MSG)
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
      INTEGER(KIND=I_P) :: ios !! Status from a `READ` or the closing `REWIND`.
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from a failed `READ` or `REWIND`.
      CHARACTER(LEN=*), PARAMETER :: location = 'oc_input:OCREAD' !! Location string for read-error reports.
      INTEGER, DIMENSION(NXEE*NYEE), SAVE :: IDUM !! Integer input workspace; scratch within this routine only. `SAVE` keeps it in static storage, as the former module variable was.
      DOUBLEPRECISION, DIMENSION(NELEE), SAVE :: DUMMY !! Floating-point input workspace; scratch within this routine only. `SAVE` keeps it in static storage, as the former module variable was.

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
      READ (OCD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (OCD, *, IOSTAT=ios, IOMSG=emsg) NT, NCATR, KONT, BIOWAT
      CALL errstat_read(ios, location, emsg)

      KKON = MOD(KONT, 2)
      BOUT = (KKON == 1)

      IF (BOUT) WRITE (FID_logfile, 9080) ' ', NCATR

      !              OC time-step data
      ! :OC2
      READ (OCD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (OCD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)

      !              Default roughness parameters & floating-point variables
      ! :OC3
      READ (OCD, *, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_read(ios, location, emsg)
      READ (OCD, *, IOSTAT=ios, IOMSG=emsg) SMIN, CDRS, TDC, TFC, DET
      CALL errstat_read(ios, location, emsg)

      IF (KONT < 2) TDC = TFC + one

      ! :OC4
      IF (ISZERO(CDRS)) THEN
         IF (NCATR > NOCTAB .OR. NCATR < 0) THEN
            WRITE (MSG, '("Number of roughness categories NCATR =",I4,2X, &
            &                         "lies outside range 0:NOCTAB = 0 :",I4)') NCATR, NOCTAB
            CALL RAISE_ERROR(ERRLVL_fatal, 1047, FID_logfile, 0, 0, MSG)
         END IF

         IF (NCATR > 0) THEN
            ! PERF FIX: Implied DO loop instead of array slice
            READ (OCD, *, IOSTAT=ios, IOMSG=emsg) (CATR(I), I=1, NCATR)
            CALL errstat_read(ios, location, emsg)
            IF (BOUT) THEN
               WRITE (FID_logfile, 9084) (CATR(I), I=1, NCATR)
               WRITE (FID_logfile, *)
            END IF
         END IF
      ELSE IF (BOUT) THEN
         WRITE (FID_logfile, 9082) CDRS
      END IF

      !              INITIAL OVERLAND FLOW ELEVATIONS
      ! :OC5
      IF (BIOWAT) THEN
         CALL AREADR(DUMMY, KKON, OCD, FID_logfile)
      ELSE
         ! PERF FIX: Explicit DO loop instead of array slice assignment
         DO ielt = NGDBGN, total_no_elements
            DUMMY(ielt) = ZERO
         END DO
         IF (BOUT) WRITE (FID_logfile, 9085) 'zero'
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
         CALL AREADR(STRXX, KKON, OCD, FID_logfile)
         CALL AREADR(STRYY, KKON, OCD, FID_logfile)
      ELSE
         ! Pass base memory address IDUM
         CALL AREADI(IDUM, KKON, OCD, FID_logfile, NCATR)

         roughness_x_loop: DO ielt = NGDBGN, total_no_elements
            ICAT = MAX(1, MIN(IDUM(ielt), NCATR))
            STRXX(ielt) = CATR(ICAT)
         END DO roughness_x_loop

         CALL AREADI(IDUM, KKON, OCD, FID_logfile, NCATR)

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
      REWIND (OCD, IOSTAT=ios, IOMSG=emsg)
      CALL errstat_rewind(ios, fid=OCD, iomsg=emsg)

      IF (IXER /= 0) THEN
         WRITE (MSG, 9412) IXER
         CALL RAISE_ERROR(ERRLVL_fatal, 1049, FID_logfile, 0, 0, MSG)
      ELSE IF (BOUT) THEN
         WRITE (FID_logfile, 9500) 'no-flow'
         IF (NOCBC > 0) WRITE (FID_logfile, 9600) 'Index', 'Element', 'Face', &
            'Type', 'Category', 'Coefficients'

         print_bc_loop: DO IBC = 1, NOCBC
            TYPEE = NOCBCD(IBC, 3)

            ! PERF FIX: Explicit indexing and Implied DO loop instead of slices
            WRITE (FID_logfile, 9610) IBC, NOCBCD(IBC, 1), NOCBCD(IBC, 2), CTYPE(TYPEE), &
               NOCBCD(IBC, 4), (COCBCD(I, IBC), I=1, NC(TYPEE))
         END DO print_bc_loop

         WRITE (FID_logfile, 9080) ' END OF '
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

END MODULE oc_input

