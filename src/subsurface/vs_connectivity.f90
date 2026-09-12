!> summary: Cell, layer and link connectivity for the subsurface columns.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[VSCONC]] builds the vertical discretisation of each element column — the
!> soil, river-bed and aquifer-zone cells and the `JVSACN`/`JVSDEL`
!> connectivity that couples neighbouring columns — and [[VSCONL]], with its
!> contained `FNCELL` helper, does the same for the channel links and their
!> banks. Both are called from [[vs_input:VSIN]] during setup, and their
!> results are what makes the one-dimensional column solve in
!> [[vs_driver]] laterally coupled.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995--1998 | GP / RAH | 4.0--4.2 | Created the VSS component and its `.INC` include groups. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the VSS Fortran sources into a single Fortran 90 module. |
!> | 2026-03 to 2026-05 | SB / SvB | 4.6 | Modernisation pass, and moved `VSREAD`'s read buffers to allocatable module state to avoid a stack-related crash. |
!> | 2026-09-10 | SvB | - | Split out of VSmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE vs_connectivity

   USE MOD_PARAMETERS, ONLY: half, zero
   USE array_limits, ONLY: LLEE, nelee, NLYREE
   USE element_geometry, ONLY: top_cell_no, total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF
   USE channel_geometry, ONLY: BEXBK, FHBED, ICMBK, NHBED, ZBEFF
   USE et_state, ONLY: initialise_eruz
   USE file_units, ONLY: FID_logfile
   USE vs_state, ONLY: DELTAZ, initialise_al_c, JVSACN, JVSDEL, NLYR, NLYRBT, NTSOIL, &
                       ZLYRBT, ZVSNOD
   USE vs_config, ONLY: DCRBED, DCRTOT, DCSTOT, DCSZON, JVSALN, NCRBED, NCSZON, NVSERR, &
                        VSZMAX, VSZMIN
   USE vs_driver, ONLY: initialise_vsmod
   USE spatial_fields, ONLY: ALSPRD
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal, ERRLVL_error, ERRLVL_warn

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: VSCONC, VSCONL

CONTAINS

!> Builds VSS cell thicknesses, node elevations, and cell connectivity.
!>
!> `VSCONC` translates the manual `VS06` soil-zone cell depths, `VS07`
!> river-bed cell depths, `VS08` aquifer-zone layer definitions, `VS09` river
!> bed geometry, and the layer connectivity prepared by [[vsconl]] into the
!> cell mesh used by the VSS solver.
!>
!> Required setup conditions include positive VSS array bounds, `VSZMAX > 0`,
!> non-negative `VSZMIN`, `NCSZON`, and `NCRBED`, `LLEE >= NCSZON`, and enough
!> model layer capacity (`NLYREE > NLYR`) for active land elements. Soil-zone
!> cell depths `DCSZON` must be at least `VSZMIN`; aquifer layer boundaries
!> must be ordered and compatible with the prescribed soil-zone depths; and
!> regular neighbour faces in `ICMREF` must reference valid elements and faces.
!> The routine is designed to be called once during VSS initialisation.
!>
!> Cell construction proceeds bottom-up. Aquifer layers are subdivided into
!> equal cells no larger than `VSZMAX`, unless later connectivity checks require
!> additional subdivision. The soil zone is then appended from the manual
!> top-down `DCSZON` depths, and link elements receive additional river-bed
!> cells from `DCRBED`. Bank elements are mirrored across each link when
!> explicit banks are enabled.
!>
!> The main outputs are:
!>
!> | Array | Meaning |
!> |:------|:--------|
!> | `DELTAZ(cell,element)` | VSS cell thickness. |
!> | `ZVSNOD(cell,element)` | Cell-node elevation. |
!> | `NLYRBT(element,layer)` | Bottom-cell index for each model layer. |
!> | `top_cell_no` / `LL` | Maximum active cell index after renumbering. |
!> | `JVSACN(face,cell,element)` | Adjacent cell connected across a face. |
!> | `JVSDEL(face,cell,element)` | Split-cell offset used when one cell connects to two neighbour cells. |
!> | `NHBED`, `FHBED` | River-bed cell index and bed fraction metadata for channel links. |
!>
!> Connectivity is first direct-matched in the soil zone and below river beds.
!> Aquifer-zone connectivity follows `JVSALN`, which encodes the layer ranges
!> allowed to exchange laterally. When two connected layer ranges have too few
!> cells to represent the required one-to-one or one-to-two split-cell
!> connections, `VSCONC` records extra layer subdivisions in `LRENUM` and
!> rebuilds the mesh. If repeated rebuilding reaches the element-count limit,
!> the routine exits through the existing fatal-error path.
!>
!> @note
!> `LRENUM` and `NRENUM` are module-lifetime state (an initialised local array
!> and a `SAVE`d counter) and therefore retain state between calls. The routine
!> also calls [[initialise_vsmod]], `INITIALISE_AL_C` and
!> [[et_state:initialise_eruz]] after each mesh-construction pass, before the
!> final rebuild test can loop back for another pass. This matches the original
!> one-call setup assumption; repeated calls, or a rebuild after allocation
!> routines that do not tolerate repeated allocation, are not safe.
!> @endnote
!>
!> @note
!> The local `nlyrmax` is declared but not used anywhere in the routine body.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written; version 4.0 completed 1996-01-17. |
!> | 1997-03-26 | RAH | 4.1 | Generic intrinsics; moved the `ERROR` calls to the end; new locals `ZAQTOP`, `ZLBOT`, `ZNODE`, `ICOL1`, `ICL0`, `NCL`; scrapped local `ZDUM1`; automatic type conversion; renamed locals `NDUM`, `ZDUM2`, `ZDUM3`, `Zasum`, `Zasum1`; replaced label/`GOTO 970` with `MAX(ZERO,VSZMIN)`; swapped the `DELTAZ`/`ZVSNOD` subscripts and moved `IBANK2`; moved a block-`IF` outside loop 974 and made it unconditional; put labels in order; defined `DELTAZ`/`ZVSNOD` for `ICL=1`; ran loop 1100 only when `ICL0>0`, called `ALINIT`, and removed loop 1170 (zeroing sub-cells); initialised `NRENUM` in `DATA`; started the `NLYRBT` search at `ICL0+1` without testing `DELTAZ>0`. |
!> | 1997-04-02 | RAH | 4.1 | Started loop 1600 at `ICOL1` instead of using a `GOTO`; rationalised the tests in loop 1500; swapped the `JVSACN`/`JVSDEL` subscripts and initialised them to `0` (previously `IUNDEF`); declared `NCELL`, `NACELL`, `ZDIFF`. |
!> | 1997-04-22 | RAH | 4.1 | Initialised `LRENUM` to `0` (previously `IUNDEF`) and tested `NCLYR<=0`. |
!> | 1997-04-23 | RAH | 4.1 | Started loop 1000 at `NLF+1` rather than testing for element type 3. |
!> | 1997-05-22 | RAH | 4.1 | Removed the "unfinished code" message and simplified a test. |
!> | 1997-05-23 | RAH | 4.1 | Set `ZVSNOD(1,IEL)` less than `ZLYRBT(IEL,1)`. |
!> | 1997-06-12 | RAH | 4.1 | Simplified loop 1120, cancelling the two preceding modifications. |
!> | 1997-07-18 | RAH | 4.1 | Renamed `ZLBOT` to `ZAQBOT`; put labels in order; used `IEL <= NLF` in place of `ITYPE == 3`; used `GOTO 1585` instead of an `ELSE`; fixed an error setting `ITOP`/`JTOP` for links (previously `LL-NCSZON`); used `NMOD` instead of `100` and merged the layer `IF`-blocks; rationalised the tests for skipping loop 1590; renamed `IALDUM`/`JALDUM` to `IRANGE`/`JRANGE`; scrapped inconsistency error 1049; fixed an aquifer-zone error (skip if either, not both). |
!> | 1997-07-28 | RAH | 4.1 | Scrapped local `IUNDEF` and arrays `LIDUM`/`LJDUM`; fixed errors in message 1037 (print `I`/`J`, not `LIDUM`/`LJDUM`, which were always 1) and at the top of the aquifer zone (`GOTO 1585`, not `1590`, for `BDONE`). |
!> | 1997-07-30 | RAH | 4.1 | Refined the split-cell treatment so splits do not straddle null cells; flagged warnings 1037 and 1053 once only; scrapped inconsistency error 1050; completed the `IEL` loop before renumbering instead of jumping out immediately. |
!> | 1997-08-01 | RAH | 4.1 | Completed the split-cell logic by spreading foregone splits (previously ill-specified); reduced the `MSG` size; simplified a test; stopped connecting the ends of river-bed cells. |
!> | 1997-08-06 | RAH | 4.1 | Added further entry conditions. |
!> | 1997-08-11 | RAH | 4.1 | Amended the `PAIR` logic to use `MISS`. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote the cell-renumbering outer loop, layer-matching search, and split-cell pairing loop from labelled `GOTO`s to `DO`/`DO WHILE` constructs with `CYCLE`/`EXIT`; replaced `CALL ALINIT` zero-initialisation with Fortran 90 array-slice assignment; converted the obsolete `FNCELL` statement function (never actually defined as a callable in the pre-modernisation source) into the contained function below; replaced the non-standard `IDIMJE` intrinsic with an equivalent `MAX(0, ...)` expression. All of these are direct control-flow/style translations with the same per-cell arithmetic. |
!> | 2026-09-12 | SvB | - | Added the `INITIALISE_ERUZ` call beside `INITIALISE_AL_C`, which no longer allocates `ERUZ` (D11). Same allocation point, same shape. |
!> @endhistory
   SUBROUTINE VSCONC()

      ! Assumed external module dependencies providing global variables:
      ! NELEE, NLYREE, LLEE, NLFEE, total_no_links, total_no_elements,
      ! top_cell_no, VSZMIN, VSZMAX, ZERO, half, DCSTOT, DCSZON, NCSZON,
      ! DCRBED, NCRBED, ZGRUND, ZLYRBT, DELTAZ, ZVSNOD, JVSACN, JVSDEL,
      ! JVSALN, NHBED, FHBED, NLYR, NLYRBT, ICMREF, ICMBK, ZBEFF, DCRTOT,
      ! INITIALISE_VSMOD, INITIALISE_AL_C, INITIALISE_ERUZ, ALSPRD, ERROR,
      ! ERRLVL_fatal, ERRLVL_warn, FID_logfile

      IMPLICIT NONE

      ! Locals
      INTEGER, PARAMETER :: JVSDUM = NELEE*NLYREE
      INTEGER :: NMOD
      INTEGER :: I, IRANGE, IBOT, IBOTL, ICL, IEL, IFA, ILYR, ITOP
      INTEGER :: J, JRANGE, JBOT, JBOTL, JCL, JEL, JFA, JLYR, JTOP
      INTEGER :: IDEL, IDEL0, IL, ILMAX, ILMIN, NITOT, NIMIN
      INTEGER :: JDEL, JDEL0, JL, JLMAX, JLMIN, NJTOT, NJMIN
      INTEGER :: IAQTOP, IBANK2, IBK, ICL0, ICL1, ICOL1, ILINK, ITYPE
      INTEGER :: DEL, JDIF, K, K2, K20, K2MOD, LCON, LTOP
      INTEGER :: NACELL, NCELL, NCL, NCLYR, NDUM, NEXTRA, NODD, NUM2
      INTEGER :: NIDUM(LLEE), NJDUM(LLEE), MAX_BOT_TOP
      DOUBLE PRECISION :: DZLYR, ZCBOT, ZDEPTH, ZBDBOT, ZCTOP, ZDUM
      DOUBLE PRECISION :: ZAQBOT, ZSZBOT, ZDIFF, ZLBOT, ZNODE
      LOGICAL :: BRENUM, BWARN, MISS, PAIR, BDONE(NELEE, 4)
      CHARACTER(LEN=57) :: MSG
      INTEGER :: nlyrmax
      INTEGER, DIMENSION(NELEE), SAVE :: IDUM !! Integer input workspace; subscripted only by element, link and bank element numbers, so `NELEE` bounds it. `SAVE` keeps it in static storage, as the former module variable was.

      ! Modern Initialization replacing DATA blocks
      INTEGER :: LRENUM(NELEE, NLYREE) = 0
      INTEGER, SAVE :: NRENUM = 0

      !----------------------------------------------------------------------*

      NMOD = NLYREE + 1

      renumbering_loop: DO
         NRENUM = NRENUM + 1

         ! Safe inline error trap replaces GOTO 8048
         IF (NRENUM > NELEE) THEN
            CALL RAISE_ERROR(ERRLVL_fatal, 1048, FID_logfile, 0, 0, 'Attempts to renumber cells have failed.')
            RETURN
         END IF

         BWARN = (NRENUM == NELEE)
         BRENUM = .FALSE.

         ! Set initial indices, dimensions & positions of cells
         !______________________________________________________*
         top_cell_no = 0

         element_loop: DO IEL = total_no_links + 1, total_no_elements
            ITYPE = ICMREF(IEL, 1)

            ! * process only grid and bank-1 elements here
            IF (ITYPE == 2) CYCLE element_loop

            ! --- loop over layers in aquifer zone (start from bottom of column)
            ZSZBOT = ZGRUND(IEL) - DCSTOT
            ICL = 1
            DELTAZ(ICL, IEL) = ZERO
            ZVSNOD(ICL, IEL) = ZERO

            layer_loop: DO ILYR = 1, NLYR(IEL)
               ! * divide each layer into equal sized cells
               ZLBOT = ZLYRBT(IEL, ILYR)
               DZLYR = MIN(ZLYRBT(IEL, ILYR + 1), ZSZBOT) - ZLBOT

               ! skip if layer is thinner than minimum cell size
               IF (DZLYR < VSZMIN) CYCLE layer_loop

               ! if no other plan make cells as large as poss but < VSZMAX
               NCLYR = LRENUM(IEL, ILYR)
               IF (NCLYR <= 0) NCLYR = MAX(1, INT(DZLYR/VSZMAX) + 1)

               ZDEPTH = DZLYR/DBLE(NCLYR)

               DO I = 1, NCLYR
                  ICL = ICL + 1
                  DELTAZ(ICL, IEL) = ZDEPTH
                  ZVSNOD(ICL, IEL) = ZDEPTH*(DBLE(I) - half) + ZLBOT
               END DO
            END DO layer_loop

            ! --- set up data for soil zone
            ZAQBOT = ZLYRBT(IEL, 1)
            ZCBOT = ZSZBOT

            DO I = NCSZON, 1, -1
               ZDEPTH = DCSZON(I)
               ZNODE = ZCBOT + ZDEPTH*half
               IF (ZNODE > ZAQBOT) THEN
                  ICL = ICL + 1
                  DELTAZ(ICL, IEL) = ZDEPTH
                  ZVSNOD(ICL, IEL) = ZNODE
               END IF
               ZCBOT = ZCBOT + ZDEPTH
            END DO

            ! --- update LL & store number of cells for this column
            top_cell_no = MAX(top_cell_no, ICL)
            IDUM(IEL) = ICL

            ! --- process link and opposite bank elements, if IEL is bank type 1
            IF (ITYPE /= 1) CYCLE element_loop

            ! * set up link cells up to bottom of link bed
            ILINK = ICMREF(IEL, 4)
            ZBDBOT = ZBEFF(ILINK) - DCRTOT
            ZCBOT = ZLYRBT(IEL, 1)

            link_cells: DO ICL1 = 1, ICL
               ZDEPTH = DELTAZ(ICL1, IEL)
               ZCTOP = ZCBOT + ZDEPTH

               IF (ZCTOP > ZBDBOT) EXIT link_cells

               DELTAZ(ICL1, ILINK) = ZDEPTH
               ZVSNOD(ICL1, ILINK) = ZVSNOD(ICL1, IEL)
               ZCBOT = ZCTOP
            END DO link_cells

            ! cell just below link bed: smaller than bank, unless ...
            ZDEPTH = ZBDBOT - ZCBOT
            IF (ZDEPTH < VSZMIN) THEN
               ! ... remainder is small: add it to the cell below
               ICL1 = ICL1 - 1
               ZDEPTH = ZDEPTH + DELTAZ(ICL1, ILINK)
            END IF

            DELTAZ(ICL1, ILINK) = ZDEPTH
            ZVSNOD(ICL1, ILINK) = ZBDBOT - half*ZDEPTH

            ! set up link bed cells
            ZCBOT = ZBDBOT
            DO I = NCRBED, 1, -1
               ZDEPTH = DCRBED(I)
               ICL1 = ICL1 + 1
               DELTAZ(ICL1, ILINK) = ZDEPTH
               ZVSNOD(ICL1, ILINK) = ZCBOT + ZDEPTH*half
               ZCBOT = ZCBOT + ZDEPTH
            END DO

            ! update LL & store number of cells for the link
            top_cell_no = MAX(top_cell_no, ICL1)
            IDUM(ILINK) = ICL1

            ! set up opposite bank cells
            IBANK2 = ICMBK(ILINK, 2)

            ! Exploit F90 array slicing
            DELTAZ(1:ICL, IBANK2) = DELTAZ(1:ICL, IEL)
            ZVSNOD(1:ICL, IBANK2) = ZVSNOD(1:ICL, IEL)

            IDUM(IBANK2) = ICL

         END DO element_loop

         ! Renumber cells & set up NLYRBT
         !____________________________________________________________________*
         IF (BEXBK) THEN
            ICOL1 = 1
         ELSE
            ICOL1 = total_no_links + 1
            NLYRBT(1:total_no_links, 1) = top_cell_no
         END IF

         ! --- loop over column elements
         DO IEL = ICOL1, total_no_elements
            NCL = IDUM(IEL)
            ICL0 = top_cell_no - NCL

            IF (ICL0 > 0) THEN
               DELTAZ(ICL0 + 1:ICL0 + NCL, IEL) = DELTAZ(1:NCL, IEL)
               ZVSNOD(ICL0 + 1:ICL0 + NCL, IEL) = ZVSNOD(1:NCL, IEL)

               DELTAZ(1:ICL0, IEL) = ZERO
               ZVSNOD(1:ICL0, IEL) = ZERO
            END IF

            ICL0 = ICL0 + 1

            DO ILYR = 1, NLYR(IEL)
               search_icl: DO ICL = ICL0 + 1, top_cell_no
                  IF (ZVSNOD(ICL, IEL) > ZLYRBT(IEL, ILYR)) EXIT search_icl
               END DO search_icl

               NLYRBT(IEL, ILYR) = ICL
               ICL0 = ICL - 1
            END DO

            NLYRBT(IEL, ILYR) = top_cell_no + 1
         END DO

         CALL INITIALISE_VSMOD()
         CALL INITIALISE_AL_C()
         CALL INITIALISE_ERUZ()

         ! Set up cell connectivities (JVSACN, JVSDEL)
         !_____________________________________________*
         DO IEL = 1, total_no_elements
            IBOT = NLYRBT(IEL, 1)
            BDONE(IEL, 1:4) = .FALSE.
            JVSACN(1:4, IBOT:top_cell_no, IEL) = 0
            JVSDEL(1:4, IBOT:top_cell_no, IEL) = 0
         END DO

         LTOP = top_cell_no - NCRBED
         IAQTOP = top_cell_no - NCSZON

         face_setup_loop: DO IEL = ICOL1, total_no_elements
            ITYPE = ICMREF(IEL, 1)
            IBOT = NLYRBT(IEL, 1)

            IF (IEL <= total_no_links) THEN
               IBK = ICMBK(IEL, 1)
               ITOP = MIN(IAQTOP + IBOT - NLYRBT(IBK, 1), LTOP)
            ELSE
               ITOP = IAQTOP
            END IF

            face_loop: DO IFA = 1, 4
               JEL = ICMREF(IEL, IFA + 4)
               IF (JEL < ICOL1) CYCLE face_loop

               JFA = ICMREF(IEL, IFA + 8)
               IF (BDONE(JEL, JFA)) CYCLE face_loop

               JBOT = NLYRBT(JEL, 1)
               JDIF = JBOT - IBOT

               ! --- channel link-bank face
               IF (IEL <= total_no_links .AND. JEL > total_no_links) THEN
                  DO ICL = IBOT, LTOP
                     JCL = ICL + JDIF
                     JVSACN(IFA, ICL, IEL) = JCL
                     JVSACN(JFA, JCL, JEL) = ICL
                  END DO

                  BDONE(IEL, IFA) = .TRUE.
                  CYCLE face_loop
               END IF

               ! --- other elements
               IF (JEL <= total_no_links) THEN
                  IBK = ICMBK(JEL, 1)
                  JTOP = MIN(IAQTOP + JBOT - NLYRBT(IBK, 1), LTOP)
                  LCON = LTOP
               ELSE
                  JTOP = IAQTOP
                  LCON = top_cell_no
               END IF

               ! ----- soil zone processing
               MAX_BOT_TOP = MAX(IBOT, JBOT)
               MAX_BOT_TOP = MAX(MAX_BOT_TOP, ITOP + 1, JTOP + 1)

               DO ICL = MAX_BOT_TOP, LCON
                  JCL = ICL
                  JVSACN(IFA, ICL, IEL) = JCL
                  JVSACN(JFA, JCL, JEL) = ICL
               END DO

               ! ----- aquifer zone processing
               ILYR = 1
               JLYR = 1

               layer_match_loop: DO WHILE (.TRUE.)
                  IBOTL = NLYRBT(IEL, ILYR)
                  JBOTL = NLYRBT(JEL, JLYR)

                  IF (IBOTL > ITOP .OR. JBOTL > JTOP) THEN
                     BDONE(IEL, IFA) = .TRUE.
                     CYCLE face_loop
                  END IF

                  JRANGE = JVSALN(IEL, ILYR, IFA)
                  IRANGE = JVSALN(JEL, JLYR, JFA)

                  IF (JRANGE == 0) THEN
                     ILYR = ILYR + 1
                     CYCLE layer_match_loop
                  ELSE IF (IRANGE == 0) THEN
                     JLYR = JLYR + 1
                     CYCLE layer_match_loop
                  END IF

                  ILMIN = IRANGE/NMOD
                  ILMAX = MOD(IRANGE, NMOD)
                  JLMIN = JRANGE/NMOD
                  JLMAX = MOD(JRANGE, NMOD)

                  ! count cells in column IEL, & no. required in JEL
                  NITOT = 0
                  NJMIN = 0
                  NODD = 0

                  DO IL = ILMIN, ILMAX
                     NCELL = FNCELL(IL, IEL, ITOP)
                     IF (JVSALN(IEL, IL, IFA) /= 0) THEN
                        DO I = 0, NCELL - 1
                           NITOT = 1 + NITOT
                           NIDUM(NITOT) = I + NLYRBT(IEL, IL)
                        END DO
                        NCELL = NCELL - NODD
                        NJMIN = (NCELL + 1)/2 + NJMIN
                        NODD = MOD(NCELL, 2)
                     ELSE IF (NCELL > 0) THEN
                        NODD = 0
                     END IF
                  END DO
                  NIDUM(NITOT + 1) = 0

                  ! count cells in column JEL, & no. required in IEL
                  NJTOT = 0
                  NIMIN = 0
                  NODD = 0

                  DO JL = JLMIN, JLMAX
                     NCELL = FNCELL(JL, JEL, JTOP)
                     IF (JVSALN(JEL, JL, JFA) /= 0) THEN
                        DO J = 0, NCELL - 1
                           NJTOT = 1 + NJTOT
                           NJDUM(NJTOT) = J + NLYRBT(JEL, JL)
                        END DO
                        NCELL = NCELL - NODD
                        NIMIN = (NCELL + 1)/2 + NIMIN
                        NODD = MOD(NCELL, 2)
                     ELSE IF (NCELL > 0) THEN
                        NODD = 0
                     END IF
                  END DO
                  NJDUM(NJTOT + 1) = 0

                  ! Checking conditions and splitting cells
                  IF (NITOT == 0 .AND. NJTOT > 0) THEN
                     WRITE (MSG, 9200) JFA, JLYR
                     IF (NRENUM == 1) CALL RAISE_ERROR(ERRLVL_warn, 1053, FID_logfile, JEL, 0, MSG)

                  ELSE IF (NJTOT == 0 .AND. NITOT > 0) THEN
                     WRITE (MSG, 9200) IFA, ILYR
                     IF (NRENUM == 1) CALL RAISE_ERROR(ERRLVL_warn, 1053, FID_logfile, IEL, 0, MSG)

                  ELSE IF (NJTOT < NJMIN) THEN
                     BRENUM = .TRUE.
                     NEXTRA = 0
                     DO JL = JLMIN, JLMAX
                        IF (JVSALN(JEL, JL, JFA) /= 0) THEN
                           IF (BWARN) THEN
                              WRITE (MSG, 9300) JFA, JL
                              CALL RAISE_ERROR(ERRLVL_warn, 1037, FID_logfile, JEL, 0, MSG)
                           END IF
                           NCELL = FNCELL(JL, JEL, JTOP)
                           NDUM = NCELL*NJMIN + NEXTRA + NJTOT/2
                           LRENUM(JEL, JL) = NDUM/NJTOT
                           NEXTRA = MOD(NDUM, NJTOT) - NJTOT/2
                        END IF
                     END DO

                  ELSE IF (NITOT < NIMIN) THEN
                     BRENUM = .TRUE.
                     NEXTRA = 0
                     DO IL = ILMIN, ILMAX
                        IF (JVSALN(IEL, IL, IFA) /= 0) THEN
                           IF (BWARN) THEN
                              WRITE (MSG, 9300) IFA, IL
                              CALL RAISE_ERROR(ERRLVL_warn, 1037, FID_logfile, IEL, 0, MSG)
                           END IF
                           NCELL = FNCELL(IL, IEL, ITOP)
                           NDUM = NCELL*NIMIN + NEXTRA + NITOT/2
                           LRENUM(IEL, IL) = NDUM/NITOT
                           NEXTRA = MOD(NDUM, NITOT) - NITOT/2
                        END IF
                     END DO

                  ELSE
                     ! how many splits possible, & how many to forego
                     IF (NITOT >= NJTOT) THEN
                        IDEL0 = 1
                        NUM2 = NITOT - NJMIN
                        NEXTRA = NJTOT - NJMIN
                     ELSE
                        IDEL0 = 0
                        NUM2 = NJTOT - NIMIN
                        NEXTRA = NITOT - NIMIN
                     END IF
                     JDEL0 = 1 - IDEL0

                     CALL ALSPRD(NEXTRA, NUM2, K20, K2MOD)

                     MISS = .FALSE.
                     K2 = -K20
                     I = 1
                     J = 1

                     pair_search: DO WHILE (I <= NITOT .AND. J <= NJTOT)
                        PAIR = (NIDUM(I + IDEL0) == NIDUM(I) + 1)
                        PAIR = (NJDUM(J + JDEL0) == NJDUM(J) + 1) .OR. PAIR
                        PAIR = .NOT. MISS .AND. PAIR

                        IF (PAIR) THEN
                           K2 = K2 + 1
                           MISS = (K2 >= 0 .AND. MOD(K2, K2MOD) == 0)
                           MISS = (K2 <= (NEXTRA - 1)*K2MOD .AND. MISS)
                           PAIR = .NOT. MISS
                        ELSE
                           MISS = .FALSE.
                        END IF

                        DEL = 0
                        IF (PAIR) DEL = 1

                        IDEL = IDEL0*DEL
                        JDEL = JDEL0*DEL

                        DO K = 0, DEL
                           ICL = NIDUM(I)
                           JCL = NJDUM(J)
                           IF (IDEL >= K) JVSACN(IFA, ICL, IEL) = JCL
                           IF (JDEL >= K) JVSACN(JFA, JCL, JEL) = ICL
                           JVSDEL(IFA, ICL, IEL) = IDEL*(1 - 2*K)
                           JVSDEL(JFA, JCL, JEL) = JDEL*(1 - 2*K)

                           ! Replaced non-standard IDIMJE with standard MAX implementation
                           I = I + MAX(0, IDEL - K)
                           J = J + MAX(0, JDEL - K)
                        END DO

                        I = I + 1
                        J = J + 1
                     END DO pair_search

                  END IF

                  ! move on to next layers
                  ILYR = ILMAX + 1
                  JLYR = JLMAX + 1

               END DO layer_match_loop

            END DO face_loop
         END DO face_setup_loop

         ! Repeat the whole thing if BRENUM was flagged
         IF (.NOT. BRENUM) EXIT renumbering_loop

      END DO renumbering_loop

      ! Finish off
      !____________*
      WRITE (FID_logfile, 9000) top_cell_no

      finish_loop: DO IEL = ICOL1, total_no_links
         IBK = ICMBK(IEL, 1)
         NACELL = LTOP + NLYRBT(IBK, 1) - NLYRBT(IEL, 1)
         ZDUM = DELTAZ(NACELL, IBK)
         ZDIFF = ZDUM - DELTAZ(LTOP, IEL)

         DELTAZ(LTOP, IEL) = ZDUM

         IF (NLYRBT(IEL, 1) <= LTOP - 1) THEN
            ZVSNOD(NLYRBT(IEL, 1):LTOP - 1, IEL) = ZVSNOD(NLYRBT(IEL, 1):LTOP - 1, IEL) - ZDIFF
         END IF

         ZVSNOD(ICL, IEL) = ZVSNOD(ICL, IEL) - ZDIFF*half

         IF (NLYR(IEL) >= 1) THEN
            ZLYRBT(IEL, 1:NLYR(IEL)) = ZLYRBT(IEL, 1:NLYR(IEL)) - ZDIFF
         END IF

         ! NB. banks 1 and 2 are identical
         NHBED(IEL, 1) = NACELL
         NHBED(IEL, 2) = NACELL
         FHBED(IEL, 1) = ZERO
         FHBED(IEL, 2) = ZERO

      END DO finish_loop

      RETURN

      ! FORMAT STATEMENTS
9000  FORMAT(/'Number of top cell in all columns (LL) = ', I3)
9200  FORMAT('Null cell connectivity being set up for face ', I1, ' layer ', I2)
9300  FORMAT('Not possible to connect all cells for face ', I1, ' layer ', I2)

   CONTAINS

      !> Returns the number of VSS cells spanned by one model layer interval.
      !>
      !> Replaces the obsolete Fortran statement function of the same name
      !> (the pre-modernisation source only commented out its definition, so
      !> `FNCELL` had never actually been a callable statement function; this
      !> contained function restores it with the same formula).
      PURE INTEGER FUNCTION FNCELL(IDX, ELEM, TOP)
         INTEGER, INTENT(IN) :: IDX  !! Model-layer index.
         INTEGER, INTENT(IN) :: ELEM !! Element number.
         INTEGER, INTENT(IN) :: TOP  !! Upper active cell bound used to clip the layer top.
         ! Calculates number of cells handling boundary constraints
         FNCELL = MAX(0, MIN(NLYRBT(ELEM, IDX + 1), TOP + 1) - NLYRBT(ELEM, IDX))
      END FUNCTION FNCELL

   END SUBROUTINE VSCONC

!> Builds the layer-to-layer lateral connectivity matrix.
!>
!> `VSCONL` builds the layer-level lateral connectivity used later by
!> [[vsconc]] to create cell-level links. It combines default aquifer-zone
!> matching with the manual `VS10`/`VS10a` user-defined aquifer connectivity
!> records (`IAQCON`).
!>
!> Required setup conditions are that the routine is called at most once per
!> run; `NAQCON` does not exceed `NVSEE` (the declared second dimension of
!> `IAQCON`); `1 <= NEL <= NELEE`, `NLF >= 0`, and `1 <= NLYR(1:NEL) <= NLYREE`;
!> and, for every element `e` from `ICOL1` to `NEL` and every face `1:4`, the
!> adjacent element `ea = ICMREF(e,4+face)` satisfies `ea <= NEL`, with
!> `1 <= ICMREF(e,8+face) <= 4` whenever `ea >= ICOL1`. If explicit banks are
!> not present, active VSS columns start at `NLF+1`; otherwise links and bank
!> elements are included from element 1.
!>
!> `JVSALN(element,layer,face)` stores the range of layers in the adjacent
!> element connected to this layer. A value of zero means no lateral connection.
!> Non-zero ranges are encoded compactly as
!>
!> \[
!>   JVSALN = NMOD\,l_{min}+l_{max},\qquad NMOD=NLYREE+1.
!> \]
!>
!> For each neighbouring element pair, `IAQCON(:,i)` records are first checked
!> for this pair. Layer numbers must be in range. Positive user records are
!> accumulated into inclusive connected-layer ranges, while conflicting
!> null/non-null records are reported as error 1038 and counted in `NVSERR`.
!>
!> Default matching starts immediately below the soil-zone depth
!> \(ZGRUND-DCSTOT\), using a small tolerance to avoid roundoff at exact layer
!> boundaries. When no user record overrides the pair, layers with the same
!> soil/lithology type are connected one-to-one. If soil types differ, the
!> routine skips downward through one or both columns until it finds the next
!> compatible soil type or user-specified connection, trying to preserve
!> continuity where possible. Boundary faces, branched channels, and link-flank
!> faces receive null connectivity.
!>
!> @warning
!> The legacy comments describe layer zero in `IAQCON` as an explicit null
!> connection, but a new one-sided zero record is not stored as a simple null
!> marker by the current range-building code. Use positive layer-pair records
!> for user-defined connectivity and do not rely on one-sided zero records to
!> block default matching.
!> @endwarning
!>
!> @note
!> The local `BDONE` array is `DATA`-initialised and retained between calls.
!> This is another reason the routine follows the original one-call setup
!> assumption. Note also that `NAQCON` and `IAQCON` carry no `INTENT`
!> attribute in the current declaration (both are read-only in this routine).
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-07-20 | GP | 4.0 | Written; version 4.0 completed 1995-08-08. |
!> | 1997-05-08 | RAH | 4.1 | New locals `ICOL1`, `JTYPE`, `LYR`, `NLYRI`; simplified the null-connectivity test and amended its comment; generic intrinsics; removed the illegal `DATA` statement for `JVSALN`. |
!> | 1997-05-22 | RAH | 4.1 | Fixed an error setting `JLMAX` (used `JLMIN`, not `ILMIN`); scrapped the "null connectivity" message (error 1047). |
!> | 1997-06-30 | RAH | 4.1 | Moved `NAQCON`/`IAQCON` from `VSINIT.INC` into the argument list and swapped their indices (see [[vsread]]). |
!> | 1997-07-03 | RAH | 4.1 | Initialised `JVSALN` to `0` (previously `IUNDEF`) once and for all, but only for active elements and only up to `NLYR(iel)+1`. |
!> | 1997-07-10 | RAH | 4.1 | Redefined `IUNDEF` (previously `9999`); used `NMOD` instead of `100`; added detail to the `ERROR` message; put labels in order; rewrote loop 110 and fixed an error there (multiply `JLYR` by `NMOD+1` on first assignment); trapped invalid `JLYR`. |
!> | 1997-07-11 | RAH | 4.1 | New local `ZSMALL`; rewrote loop 200 and fixed errors there: set `JVSALN` on both sides for user-defined connectivity, corrected the expressions for `ILMIN` and similar, and generalised the default strategy (previously it checked/set a single embedded layer, missing some, else matched soils, else moved down a layer). Used `-1` for `IUNDEF`. |
!> | 1997-07-14 | RAH | 4.1 | Left bank-link faces at zero (never used anyway); moved the loop 200 criterion to the start (previously at the end); set `ISOILP=0` for `ILYR=NLYRI` and used `JSOILP`. |
!> | 1997-07-21 | RAH | 4.1 | Made `JVSALN` always either `0` or `NMOD*imin+imax`. |
!> | 1997-08-13 | RAH | 4.1 | Stopped giving up on a face when no match is found for `ILYR`/`JLYR`. |
!> | 2026-04-06/07 | SvB | 4.6 | Rewrote labelled `GOTO` loops (default-connectivity initialisation, per-pair layer counters, the layer-matching search, and the connected-range walk) as `DO`/`DO WHILE` constructs with `CYCLE`/`EXIT`; no change to the matching arithmetic. |
!> @endhistory
   SUBROUTINE VSCONL(NAQCON, IAQCON)

! Input arguments

      INTEGER :: NAQCON       !! Number of user-defined aquifer connectivity records.
      INTEGER :: IAQCON(4, *)  !! User aquifer connectivity records: element/layer pairs for adjacent columns.
! Locals, etc
!INTRINSIC MAX, MIN, MOD
      INTEGER :: NMOD
      DOUBLEPRECISION ZSMALL
      PARAMETER(NMOD=NLYREE + 1, ZSMALL=1D-6)
      INTEGER :: I, J, ILYR, JLYR, IEL, JEL, IFA, JFA, NLYRI, NLYRJ
      INTEGER :: ILMIN, ILMAX, JLMIN, JLMAX, IRANGE, JRANGE, ISOIL, &
                 JSOIL
      INTEGER :: ISKIP, JSKIP, ISOILP, JSOILP, I1, I2, ICOL1, K, KEL
      INTEGER :: ILDUM(NLYREE), JLDUM(NLYREE)
      DOUBLEPRECISION ZSZBOT
      LOGICAL :: IOK, MOVEJ, TEST1, BDONE(NELEE)
      CHARACTER(LEN=132) :: MSG

      DATA BDONE/NELEE*.FALSE./
!----------------------------------------------------------------------*
      IF (BEXBK) THEN
         ICOL1 = 1
      ELSE
         ICOL1 = total_no_links + 1

      END IF
! ----- default is null connectivity
      DO IFA = 1, 4
         DO IEL = 1, total_no_elements
            DO ILYR = 1, NLYR(IEL) + 1
               JVSALN(IEL, ILYR, IFA) = 0
            END DO
         END DO
      END DO

! Main loop over (faces of) column elements
!___________________________________________*

      element_loop: DO IEL = ICOL1, total_no_elements
         NLYRI = NLYR(IEL)

         face_loop: DO IFA = 1, 4
            JEL = ICMREF(IEL, IFA + 4)
            ! null connectivity for boundary faces, branched channels & link flanks

            ! 1. Skip rest of loop if face already processed using CYCLE
            IF (JEL < ICOL1 .OR. (IEL <= total_no_links .AND. JEL > total_no_links)) CYCLE face_loop
            IF (BDONE(JEL)) CYCLE face_loop

            ! ... else process BOTH sides of face
            NLYRJ = NLYR(JEL)
            JFA = ICMREF(IEL, IFA + 8)

            ! 2. Replaced the 102 and 104 loops with array slicing
            ILDUM(1:NLYRI) = -1
            JLDUM(1:NLYRJ) = -1

            aqcon_loop: DO I = 1, NAQCON
               I1 = IAQCON(1, I)
               I2 = IAQCON(3, I)

               ! * does entry I belong to the current pair of elements?
               IF (IEL == I1 .AND. JEL == I2) THEN
                  K = 2
               ELSEIF (IEL == I2 .AND. JEL == I1) THEN
                  K = 4
               ELSE
                  ! 3. Replaced GOTO 110 with CYCLE
                  CYCLE aqcon_loop
               END IF

               ILYR = IAQCON(K, I)
               JLYR = IAQCON(6 - K, I)
               MSG = ' '

               IF (ILYR < 0 .OR. ILYR > NLYRI) THEN
                  ! * ILYR out of range
                  KEL = IEL
                  WRITE (MSG, 9381) ILYR, I, IEL, NLYRI
               ELSEIF (JLYR < 0 .OR. JLYR > NLYRJ) THEN
                  ! * JLYR out of range
                  KEL = JEL
                  WRITE (MSG, 9381) JLYR, I, JEL, NLYRJ
               ELSE
                  IF (ILYR > 0) THEN
                     JRANGE = ILDUM(ILYR)
                     TEST1 = JLYR == 0 .AND. JRANGE > 0
                     IF (JRANGE == 0 .OR. TEST1) THEN
                        ! * invalid
                        KEL = IEL
                        JRANGE = MOD(JLYR + JRANGE, NMOD)
                        WRITE (MSG, 9382) IEL, ILYR, JRANGE, JEL, I
                     ELSE
                        IF (JRANGE < 0) JRANGE = NMOD*NLYRJ + 1
                        JLMIN = MIN(JLYR, JRANGE/NMOD)
                        JLMAX = MAX(JLYR, MOD(JRANGE, NMOD))
                        ILDUM(ILYR) = NMOD*JLMIN + JLMAX
                     END IF
                  END IF

                  IF (JLYR > 0) THEN
                     IRANGE = JLDUM(JLYR)
                     TEST1 = ILYR == 0 .AND. IRANGE > 0
                     IF (IRANGE == 0 .OR. TEST1) THEN
                        ! * invalid
                        KEL = JEL
                        IRANGE = MOD(ILYR + IRANGE, NMOD)
                        WRITE (MSG, 9382) JEL, JLYR, IRANGE, IEL, I
                     ELSE
                        IF (IRANGE < 0) IRANGE = NMOD*NLYRI + 1
                        ILMIN = MIN(ILYR, IRANGE/NMOD)
                        ILMAX = MAX(ILYR, MOD(IRANGE, NMOD))
                        JLDUM(JLYR) = NMOD*ILMIN + ILMAX
                     END IF
                  END IF
               END IF

               ! * note: MSG for ILYR>0.and.JRANGE=0 is lost
               ! * if also JLYR>0.and.IRANGE=0
               IF (MSG /= ' ') THEN
                  CALL RAISE_ERROR(ERRLVL_error, 1038, FID_logfile, KEL, 0, MSG)
                  NVSERR = NVSERR + 1
               END IF
            END DO aqcon_loop

            ! set ILYR & JLYR to numbers of layers immediately below soil zone
            ZSZBOT = ZGRUND(IEL) - DCSTOT - ZSMALL

            ! 4. Replaced 120 and 140 loops with EXIT searches
            find_ilyr: DO ILYR = NLYRI, 1, -1
               IF (ZLYRBT(IEL, ILYR) < ZSZBOT) EXIT find_ilyr
            END DO find_ilyr

            ZSZBOT = ZGRUND(JEL) - DCSTOT - ZSMALL

            find_jlyr: DO JLYR = NLYRJ, 1, -1
               IF (ZLYRBT(JEL, JLYR) < ZSZBOT) EXIT find_jlyr
            END DO find_jlyr

            ! --- start of loop over layers (downwards from top of aquifer zone)
            ! 5. Replaced the massive 200 GOTO loop with a DO WHILE
            layer_matching: DO WHILE (ILYR > 0 .AND. JLYR > 0)
               ISOIL = NTSOIL(IEL, ILYR)
               JSOIL = NTSOIL(JEL, JLYR)
               JRANGE = ILDUM(ILYR)
               IRANGE = JLDUM(JLYR)

               IF (JRANGE == 0 .OR. (IRANGE > 0 .AND. JRANGE < 0)) THEN
                  ! * null
                  ILYR = ILYR - 1
               ELSEIF (IRANGE == 0 .OR. (JRANGE > 0 .AND. IRANGE < 0)) THEN
                  ! * null
                  JLYR = JLYR - 1
               ELSEIF (JRANGE > 0) THEN
                  ! * user-specified
                  JLMIN = JRANGE/NMOD
                  ILMIN = IRANGE/NMOD

                  ! 6. Replaced the 210 GOTO jump with another DO WHILE
                  ! * repeat until the whole connected range is processed
                  process_range: DO WHILE (ILMIN <= ILYR)
                     ILMAX = ILYR
                     DO ILYR = ILMAX, ILMIN, -1
                        JRANGE = ILDUM(ILYR)
                        JVSALN(IEL, ILYR, IFA) = MAX(0, JRANGE)
                        IF (JRANGE > 0) JLMIN = MIN(JLMIN, JRANGE/NMOD)
                     END DO

                     JLMAX = JLYR
                     DO JLYR = JLMAX, JLMIN, -1
                        IRANGE = JLDUM(JLYR)
                        JVSALN(JEL, JLYR, JFA) = MAX(0, IRANGE)
                        IF (IRANGE > 0) ILMIN = MIN(ILMIN, IRANGE/NMOD)
                     END DO
                  END DO process_range

               ELSEIF (ISOIL == JSOIL) THEN
                  ! * matching soils
                  JVSALN(IEL, ILYR, IFA) = JLYR*NMOD + JLYR
                  JVSALN(JEL, JLYR, JFA) = ILYR*NMOD + ILYR
                  ILYR = ILYR - 1
                  JLYR = JLYR - 1
               ELSE
                  ! * decide whether to move down column IEL or JEL:
                  ! * set type of soil above
                  ISOILP = 0
                  IF (ILYR < NLYRI) ISOILP = NTSOIL(IEL, ILYR + 1)
                  JSOILP = 0
                  IF (JLYR < NLYRJ) JSOILP = NTSOIL(JEL, JLYR + 1)

                  ! * look for next matching soil or user-specification
                  search_i: DO I = ILYR - 1, 1, -1
                     IF (NTSOIL(IEL, I) == JSOIL .OR. ILDUM(I) >= 0) EXIT
                  END DO search_i
                  ISKIP = ILYR - I

                  search_j: DO J = JLYR - 1, 1, -1
                     IF (NTSOIL(JEL, J) == ISOIL .OR. JLDUM(J) >= 0) EXIT
                  END DO search_j
                  JSKIP = JLYR - J

                  ! * choose smallest skip; or preserve soil continuity
                  MOVEJ = (ISOIL == ISOILP) .OR. (JSOIL /= JSOILP)
                  MOVEJ = (JSKIP < ISKIP) .OR. (JSKIP == ISKIP .AND. MOVEJ)
                  MOVEJ = (J > 0) .AND. MOVEJ

                  IF (MOVEJ) MOVEJ = JLDUM(J) < 0

                  ! * would there be any point moving down IEL?
                  IOK = I > 0
                  IF (IOK) IOK = ILDUM(I) < 0

                  ! * the choice is made
                  IF (MOVEJ) THEN
                     JLYR = J
                  ELSEIF (IOK) THEN
                     ILYR = I
                  ELSE
                     ILYR = ILYR - 1
                     JLYR = JLYR - 1
                  END IF
               END IF
            END DO layer_matching
            ! * process next pair of layers happens naturally by looping the WHILE

         END DO face_loop

         BDONE(IEL) = .TRUE.

      END DO element_loop

! Formats
!_________*
9381  FORMAT('Layer', I3, ' out of range, IAQCON entry', I3, &
           &      ' (element', I5, ' has', I3, ' layers)')

9382  FORMAT('Invalid null connection, element', I5, ':', &
           &      ' layer', I3, ' already connected to layer', I3, ', element', I5, &
           &      ' (see IAQCON entry', I3, ')')
   END SUBROUTINE VSCONL

END MODULE vs_connectivity

