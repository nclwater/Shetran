!> summary: The `CM1`--`CM61` contaminant input records and the component setup.
!> author: GP, Newcastle University; RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University
!>
!> [[CMRD]] reads manual records `CM1`--`CM61` into the parameter and state
!> modules; [[INCM]] is the frame-level setup routine that allocates the
!> component's state, calls `CMRD` and establishes the initial concentration
!> profiles and the three-size-class sediment fallback used when sediment
!> transport is disabled. `MUERR2` reports the component's numbered input
!> diagnostics and stays here rather than in a global error module.
!>
!> `INCM` and `MUERR2` were in `FRmod`; `INCM` is public because frame
!> initialisation calls it.
!>
!> @warning
!> Manual fields `CM57`, `CM59` and `CM61` are read by `CMRD` into local arrays
!> which are discarded on return. Consequently [[cm_column:PHI]] still returns
!> `0.5D0` and [[cm_column:DISP]] still returns `3.0D-8`, irrespective of those
!> input records.
!>
!> `CMRD` also declares local `ISFLXB` and `ISADNL` variables. They shadow the
!> same-named flags in [[cm_solver_flags]], leaving the module flags later read
!> by the solvers undefined under standard Fortran. `ISPLT` likewise has no
!> current assignment. This documentation records current behaviour; it does not
!> repair those runtime defects.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993--1998 | GP / RAH / SB | 3.4--4.2 | Developed and reorganised the contaminant transport routines. |
!> | 2008-12 | JE | 4.3.5F90 | Created `CMmod` while converting the former CM `COLM` and `LINK` Fortran sources to Fortran 90. |
!> | 2020-03-05 | SvB | - | Replaced the complete `SGLOBAL` include with selected imports. |
!> | 2026-09-10 | SvB | - | Split out of CMmod, FRmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE cm_input

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, half, one, zero, zero1
   USE array_limits, ONLY: LLEE, NCONEE, nelee, nlfee, NOCTAB, NSEDEE, NSEE, nxee, nyee
   USE element_geometry, ONLY: cellarea, top_cell_no, total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF, ICMXY, NX, NY
   USE channel_geometry, ONLY: BEXBK, CLENTH, CWIDTH, FHBED, ICMBK, LINKNS, NHBED
   USE file_units, ONLY: CMD, CMP
   USE input_workspace, ONLY: DUMMY, IDUM
   USE et_state, ONLY: PNETTO
   USE vs_state, ONLY: DELTAZ, JVSACN, JVSDEL, NLYRBT, NS, NTSOIL, QVSH, QVSV, VSPOR, VSTHE, &
                       ZVSNOD
   USE oc_state, ONLY: ARXL, QOC
   USE sy_state, ONLY: ARBDEP, DLS, FBETA, FBTSD, FDEL, GINFD, GINFS, GNU, GNUBK, NSED, &
                       NSOBED, PBSED, QDEFF, SOFN, SOSDFN
   USE cm_parameters, ONLY: ALPHA, ALPHBD, ALPHBS, CCAPB, CCAPE, CCAPI, CCAPIN, CCAPIO, &
                            CCAPR, CCCC, CCCCO, FADS, GCPLA, GGLMSO, GNN, IIICF, IIICFO, &
                            KDDLS, KDDSOL, NCON, SSSS, SSSSO
   USE cm_bank_geometry, ONLY: FNCEBD, NBANK, NCEAB, NCEBD
   USE cm_column_geometry, ONLY: JKZCOL, JOLFN, NCOLMB, NOL, NOLBT, NOLCE, NOLCEA, OODO, &
                                 SCL, ZCOLMB
   USE cm_column_previous, ONLY: DSWO, GGAMMO, QIO, QQO, QQQSWO, QQRFO, RSZWLO, UUAJPO, &
                                 VSTHEO, ZONEO
   USE cm_column_scaling, ONLY: D0, NCETOP, OMSGMA, SGMA, SGSQ, Z2, Z2OD, Z2SQ, Z2SQOD
   USE cm_link_water, ONLY: ACPBDO, ACPBI, ACPBSG, ACPSFO, DBDI, DBS, THBED, THBEDO
   USE cm_sediment_previous, ONLY: DLSO, FBBEDO, FBTSDO, FDELO, GNUO
   USE cm_solver_flags, ONLY: ISADNL, ISFLXB, ISPLT
   USE cm_plant, ONLY: INPL
   USE input_validation, ONLY: ALCHK, ALCHKI
   USE interpolation, ONLY: ALINTP
   USE record_readers, ONLY: ALRED2, ALREDC, ALREDF, ALREDI, ALREDL
   USE spatial_fields, ONLY: ALALLI
   USE linear_algebra, ONLY: dcopy
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal, ERRLVL_error
   USE error_status, ONLY: errstat_alloc
   USE OCmod2, ONLY: GETHRF

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: CMRD, INCM

CONTAINS

!> @brief Reads the contaminant data file and initialises contaminant controls.
!>
!> The record sequence is the one documented in the user manual's
!> *Contaminant Migration Components* section:
!>
!> | Records | Current destination |
!> |:--------|:--------------------|
!> | `CM1`--`CM5` | Title, `NCON`, and the local base-boundary selector `ISFLXB`. |
!> | `CM7`--`CM11` | Default and exceptional bottom contaminant cells in `NCOLMB`; `-1` selects `NLYRBE`. |
!> | `CM13`--`CM23` | Local nonlinear flag, bed depths, and property-table counts. |
!> | `CM25`--`CM26e` | Uniform or category/depth-dependent initial concentrations. |
!> | `CM27`--`CM39` | Rain, external-flow, base, and dry-deposition boundary data. |
!> | `CM41`--`CM55` | Soil fractions, reaction/exchange constants, distribution coefficients, and adsorption-site fractions. |
!> | `CM57`--`CM61` | Local mobile-water, diffusion, and dispersivity tables which are read but not retained. |
!>
!> With the local `ISFLXB` true, `CM33` and `CM37` populate `CCAPR`, the
!> concentration convected by base flux. Otherwise they populate the prescribed
!> base-cell concentration `CCAPB`. Spatial initial conditions retain the link
!> default in `CCAPIN`; the assignment of link entries in `NCATTY` remains
!> commented out in current code.
!>
!> `CMD` is opened/closed through `ALRED2`; the title is echoed on `CPR`.
!> Invalid dimensions, element/soil/contaminant indices, or workspace demands
!> call `ERROR` with fatal codes 2101, 2102, or 3001--3008. `IDUM` and `DUMMY`
!> are caller-owned work arrays and their contents are not preserved.
!>
!> @warning The local `ISFLXB` and `ISADNL` declarations shadow the flags in
!> [[cm_solver_flags]]. Only the former affects this read routine; neither value reaches
!> the later transport solvers. Likewise `PHIDAT`, `DIFDAT`, and `DISPDT` are
!> local arrays and are discarded. See [[phi]] and [[disp]].
!> @endwarning
!>
!> @note The manual requires `DBDI>DBS` and also records an unexplained legacy
!> restriction that `DBDI` must not equal twice `DBI`. This routine reads both
!> bed depths but validates neither condition.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995-02-01 | RAH | 3.4.2 | Created the CM reader represented by this routine. |
!> | 1995-03-22 | RAH | 3.4.2 | Recorded the routine in the legacy modification header. |
!> | 1997-05-01 | SB | 4.2 | Added the spatially distributed initial-condition input path. |
!> @endhistory
   SUBROUTINE CMRD(CMD, CPR, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, NEL, NLF, NLFEE, NSEE, NS, &
                   NSEDEE, NSED, MAX_NUM_DATA_PAIRS, NX, NXEE, NYEE, NY, NLYRBE, ICMXY, ICMBK, &
                   ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, NCATTY, NCON, NCOLMB, NTAB, DBS, &
                   DBDI, CCAPI, CCAPE, CCAPR, CCAPB, TABLE_CONCENTRATION, TABLE_WATER_DEPTH, &
                   IIICF, SOFN, GNN, GGLMSO, ALPHBD, ALPHBS, KDDLS, ALPHA, FADS, ISCNSV, IDUM, &
                   DUMMY)

      USE cm_parameters, ONLY: CCAPIN

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: CMD                    !! CM input unit.
      INTEGER, INTENT(IN) :: CPR                    !! CM print and diagnostic unit.
      INTEGER, INTENT(IN) :: MAX_NUM_CATEGORY_TYPES !! Allocated maximum number of spatial categories.
      INTEGER, INTENT(IN) :: NCONEE                 !! Allocated contaminant dimension.
      INTEGER, INTENT(IN) :: NELEE                  !! Allocated element/workspace dimension.
      INTEGER, INTENT(IN) :: NEL                    !! Active element count.
      INTEGER, INTENT(IN) :: NLF                    !! Active channel-link count.
      INTEGER, INTENT(IN) :: NLFEE                  !! Allocated channel-link dimension.
      INTEGER, INTENT(IN) :: NSEE                   !! Allocated soil-type dimension.
      INTEGER, INTENT(IN) :: NS                     !! Active soil-type count.
      INTEGER, INTENT(IN) :: NSEDEE                 !! Allocated sediment-fraction dimension.
      INTEGER, INTENT(IN) :: NSED                   !! Active sediment-fraction count.
      INTEGER, INTENT(IN) :: MAX_NUM_DATA_PAIRS     !! Allocated depth/concentration pairs per category.
      INTEGER, INTENT(IN) :: NX                     !! Active grid-cell count in the x direction.
      INTEGER, INTENT(IN) :: NXEE                   !! Allocated grid dimension in the x direction.
      INTEGER, INTENT(IN) :: NYEE                   !! Allocated grid dimension in the y direction.
      INTEGER, INTENT(IN) :: NY                     !! Active grid-cell count in the y direction.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)        !! Grid-cell to element-number map.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)        !! Link to left/right bank-element map.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Element reference map passed to the category reader.
      INTEGER, INTENT(IN) :: NLYRBE(NLF + 1:NEL)    !! Base VSS layer for every land column.
      LOGICAL, INTENT(IN) :: BEXBK                  !! True when explicit bank elements are present.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)          !! True for north--south channel links.

      ! Output arguments
      INTEGER, INTENT(OUT) :: NCON !! Number of simulated contaminants read from `CM3`.
      INTEGER, INTENT(OUT) :: NUM_CATEGORIES_TYPES(NCONEE) !! Spatial category count by contaminant.
      INTEGER, INTENT(OUT) :: NCATTY(NELEE, NCONEE) !! Spatial category by element and contaminant.
      INTEGER, INTENT(OUT) :: NCOLMB(NLF + 1:NEL) !! Bottom contaminant cell by land column.
      INTEGER, INTENT(OUT) :: NTAB(MAX_NUM_CATEGORY_TYPES, NCONEE) !! Depth-table length by category and contaminant.
      DOUBLE PRECISION, INTENT(OUT) :: DBS !! Depth to the base of the bed-surface layer (m).
      DOUBLE PRECISION, INTENT(OUT) :: DBDI !! Initial depth to the base of the deep-bed layer (m).
      DOUBLE PRECISION, INTENT(OUT) :: CCAPI(NCONEE) !! Rainfall concentration by contaminant.
      DOUBLE PRECISION, INTENT(OUT) :: CCAPE(NELEE, NCONEE) !! External-flow concentration by element and contaminant.
      DOUBLE PRECISION, INTENT(OUT) :: CCAPR(NELEE, NCONEE) !! Concentration convected through a flux base boundary.
      DOUBLE PRECISION, INTENT(OUT) :: CCAPB(NELEE, NCONEE) !! Prescribed base-cell concentration.
      DOUBLE PRECISION, INTENT(OUT) :: &
         TABLE_CONCENTRATION(MAX_NUM_CATEGORY_TYPES, &
                             MAX_NUM_DATA_PAIRS, NCONEE) !! Initial concentration by category/depth/contaminant.
      DOUBLE PRECISION, INTENT(OUT) :: &
         TABLE_WATER_DEPTH(MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCONEE) !! Depth paired with each spatial concentration.
      DOUBLE PRECISION, INTENT(OUT) :: IIICF(NCONEE) !! Dry-deposition rate by contaminant.
      DOUBLE PRECISION, INTENT(OUT) :: SOFN(NSEE, 3) !! Three default sediment-size fractions by soil type.
      DOUBLE PRECISION, INTENT(OUT) :: GNN(NCONEE) !! Freundlich isotherm power by contaminant.
      DOUBLE PRECISION, INTENT(OUT) :: GGLMSO(NCONEE) !! Chemical decay coefficient by contaminant.
      DOUBLE PRECISION, INTENT(OUT) :: ALPHBD(NCONEE) !! Exchange coefficient between the two channel-bed layers.
      DOUBLE PRECISION, INTENT(OUT) :: ALPHBS(NCONEE) !! Exchange coefficient between channel water and bed surface.
      DOUBLE PRECISION, INTENT(OUT) :: &
         KDDLS(NSEDEE, NCONEE) !! Reference distribution coefficient by sediment fraction and contaminant.
      DOUBLE PRECISION, INTENT(OUT) :: ALPHA(NSEE, NCONEE) !! Dynamic/dead-space soil exchange coefficient.
      DOUBLE PRECISION, INTENT(OUT) :: FADS(NSEE, NCONEE) !! Fraction of adsorption sites in the dynamic region.
      LOGICAL, INTENT(OUT) :: ISCNSV(NCONEE) !! True where spatial initial concentrations are read for a contaminant.

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT) :: IDUM !! Integer input workspace; contents are overwritten.
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY !! Real input workspace; contents are overwritten.

      ! Locals, etc
      INTEGER, PARAMETER :: FATAL = 1

      INTEGER :: rubbish(1, 1), j
      INTEGER :: I, IEL, INDX, NC, NCBC, NCED, NCLBND, NCONCM, NCONT
      INTEGER :: NDATA, NFEX, NMAX(3), NREQ, NSCM, NSEDCM, NTB, NTBL, SOIL
      LOGICAL :: LDUM(1) !! One-value logical input buffer.
      LOGICAL :: ISFLXB  !! Local `CM5` flag; shadows and does not assign [[cm_solver_flags]]'s flag.
      LOGICAL :: ISADNL  !! Local `CM13` flag; shadows and does not assign [[cm_solver_flags]]'s flag.
      CHARACTER(80)  :: CDUM(1)
      CHARACTER(132) :: MSG

      DOUBLE PRECISION :: PHIDAT(NSEE)        !! `CM57` mobile-water fractions, discarded on return.
      DOUBLE PRECISION :: DIFDAT(NCONEE)      !! `CM59` diffusion coefficients, discarded on return.
      DOUBLE PRECISION :: DISPDT(NSEE, NCONEE) !! `CM61` dispersivities, discarded on return.

      !----------------------------------------------------------------------*

      ! Preliminaries
      ! -------------
      !
      ! * Check status of data file
      CALL ALRED2(0, CMD, CPR, 'CMD')

      ! * Print title for contaminant simulation
      CALL ALREDC(0, CMD, CPR, ':CM1', 1, 1, CDUM)
      WRITE (CPR, '(/1X,A/)') CDUM(1)

      ! Some Static Data
      ! ----------------
      !
      ! * Number of contaminants
      CALL ALREDI(0, CMD, CPR, ':CM3', 1, 1, IDUM)
      NCON = IDUM(1)

      ! * Flux boundary condition at base of column?
      CALL ALREDL(0, CMD, CPR, ':CM5', 1, 1, LDUM)
      ISFLXB = LDUM(1)

      ! Bottom Cell Data
      ! ----------------
      !
      ! * Default cell number at base of columns (-1 special: see below)
      CALL ALREDI(0, CMD, CPR, ':CM7', 1, 1, IDUM)
      NCED = IDUM(1)

      ! * Number of columns where bottom cell number is not default value
      CALL ALREDI(0, CMD, CPR, ':CM9', 1, 1, IDUM)
      NCLBND = IDUM(1)

      IF (NCLBND > 0) THEN
         ! * Column numbers & bottom cell numbers for those columns
         NREQ = 2*NCLBND
         IF (NREQ > NELEE) THEN
            WRITE (MSG, 9809) NELEE, NREQ, 'non-default columns', 'CM9: NCLBND ', NCLBND
            CALL RAISE_ERROR(FATAL, 3001, CPR, 0, 0, MSG)
         END IF
         CALL ALREDI(0, CMD, CPR, ':CM11', 2, NCLBND, IDUM)
      END IF

      ! * Assemble the above information: set the default ...
      DO IEL = NLF + 1, NEL
         IF (NCED == -1) THEN
            ! * special case
            NCOLMB(IEL) = NLYRBE(IEL)
         ELSE
            NCOLMB(IEL) = NCED
         END IF
      END DO

      ! ... then overwrite any non-default columns
      INDX = 1
      DO I = 1, NCLBND
         IEL = IDUM(INDX)
         IF (IEL <= NLF .OR. IEL > NEL) THEN
            WRITE (MSG, 9811) IEL, 'CM11', 'column element'
            CALL RAISE_ERROR(FATAL, 3002, CPR, 0, 0, MSG)
         END IF
         NCOLMB(IEL) = IDUM(INDX + 1)
         INDX = INDX + 2
      END DO

      ! More Static & Initialization Data
      ! ---------------------------------
      !
      ! * Non-linear adsorption?
      CALL ALREDL(0, CMD, CPR, ':CM13', 1, 1, LDUM)
      ISADNL = LDUM(1)

      ! * Depth of bed surface layer
      CALL ALREDF(0, CMD, CPR, ':CM15', 1, 1, DUMMY)
      DBS = DUMMY(1)

      ! * Initial depth of bed deep layer
      CALL ALREDF(0, CMD, CPR, ':CM17', 1, 1, DUMMY)
      DBDI = DUMMY(1)

      ! Local Data
      ! ----------
      !
      ! * Number of contaminants for which there are property data
      CALL ALREDI(0, CMD, CPR, ':CM19', 1, 1, IDUM)
      NCONCM = IDUM(1)

      ! * Number of soil types for which there are contaminant data
      CALL ALREDI(0, CMD, CPR, ':CM21', 1, 1, IDUM)
      NSCM = IDUM(1)

      ! * Number of sediment sizes for which there are contaminant data
      CALL ALREDI(0, CMD, CPR, ':CM23', 1, 1, IDUM)
      NSEDCM = IDUM(1)

      ! * Set maximum admissible values for the above
      NMAX(1) = MIN(NCONEE, NELEE)
      nmax(2) = nsee
      nmax(3) = nsedee

      ! Initial Conditions
      ! ------------------
      !
      IF (NCONCM < 1 .OR. NCONCM > NMAX(1)) THEN
         WRITE (MSG, 9819) 'contaminants', 'CM19: NCONCM', NCONCM, NMAX(1)
         CALL RAISE_ERROR(FATAL, 3003, CPR, 0, 0, MSG)
      END IF

      DO I = 1, NCONCM

         ! Is the initial contaminant concentration spatially variable ?
         CALL ALREDL(0, CMD, CPR, ':CM25', 1, 1, LDUM)
         ISCNSV(I) = LDUM(1)

         IF (.NOT. ISCNSV(I)) THEN
            ! * Initial concentration throughout catchment
            CALL ALREDF(0, CMD, CPR, ':CM26', 1, 1, CCAPIN(I))
         ELSE
            ! * Initial concentration for link elements
            CALL ALREDF(0, CMD, CPR, ':CM26a', 1, 1, CCAPIN(I))
            DO J = 1, NLF
               !"" NCATTY (J, I) = CCAPIN (I)  !AD
            END DO

            ! * Find out how many typical element categories
            CALL ALREDI(0, CMD, CPR, ':CM26b', 1, 1, IDUM)
            NUM_CATEGORIES_TYPES(I) = IDUM(1)

            IF ((NUM_CATEGORIES_TYPES(I) > MAX_NUM_CATEGORY_TYPES) .OR. &
                (NUM_CATEGORIES_TYPES(I) <= 0)) THEN
               CALL RAISE_ERROR(FATAL, 2101, CPR, 0, 0, 'Error in NUM_CATEGORIES_TYPES in :CM26 in CM data file')
            END IF

            ! * Read the category type for each element into the element number
            CALL ALALLI(NUM_CATEGORIES_TYPES(I), CMD, CPR, ':CM26c', NEL, NLF, NX, NY, NELEE, &
                        NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NCATTY(NLF + 1, I), &
                        IDUM)

            ! * Table of values for each typical element
            DO NC = 1, NUM_CATEGORIES_TYPES(I)
               CALL ALREDI(0, CMD, CPR, ':CM26d', 1, 1, rubbish)
               ntbl = rubbish(1, 1)

               NTAB(NC, I) = NTBL
               IF ((NTBL > MAX_NUM_DATA_PAIRS) .OR. (NTBL <= 0)) THEN
                  CALL RAISE_ERROR(FATAL, 2102, CPR, 0, 0, 'Error in NTBL in :CM26a in CM data file')
               END IF

               NDATA = NTBL*2
               CALL ALREDF(0, CMD, CPR, ':CM26e', NDATA, 1, DUMMY)

               DO NTB = 1, NTBL
                  TABLE_WATER_DEPTH(NC, NTB, I) = DUMMY(2*NTB - 1)
                  TABLE_CONCENTRATION(NC, NTB, I) = DUMMY(2*NTB)
               END DO
            END DO
         END IF

      END DO

      ! Boundary Conditions
      ! -------------------
      !
      ! * Concentrations in rainfall
      CALL ALREDF(0, CMD, CPR, ':CM27', NCONCM, 1, CCAPI)

      ! * Number of columns which receive flow from outside catchment
      CALL ALREDI(0, CMD, CPR, ':CM29', 1, 1, IDUM)
      NFEX = IDUM(1)

      IF (NFEX > 0) THEN
         ! * Numbers of those columns, and concentrations in the flows
         ! * (read list index as extra column of floating-point data)
         NREQ = (1 + NCONCM)*NFEX
         IF (NREQ > NELEE) THEN
            WRITE (MSG, 9809) NELEE, NREQ, 'flow-receiving columns', 'CM29: NFEX', NFEX
            CALL RAISE_ERROR(FATAL, 3001, CPR, 0, 0, MSG)
         END IF
         CALL ALREDF(0, CMD, CPR, ':CM31', 1 + NCONCM, NFEX, DUMMY)
      END IF

      ! * Assemble the above info
      ! Replaced ALINIT with array slices
      CCAPE(NLF + 1:NEL, 1:NCONCM) = 0.0D0

      INDX = 1
      DO I = 1, NFEX
         IEL = NINT(DUMMY(INDX))
         IF (IEL <= NLF .OR. IEL > NEL) THEN
            WRITE (MSG, 9811) IEL, 'CM31', 'column element'
            CALL RAISE_ERROR(FATAL, 3002, CPR, 0, 0, MSG)
         END IF
         CALL DCOPY(NCONCM, DUMMY(INDX + 1), 1, CCAPE(IEL, 1), NELEE)
         INDX = INDX + 1 + NCONCM
      END DO

      ! * Default concentration at or convected into bases of columns
      CALL ALREDF(0, CMD, CPR, ':CM33', NCONCM, 1, DUMMY)

      ! Replaced ALINIT with array slices
      DO NCONT = 1, NCONCM
         IF (ISFLXB) THEN
            CCAPR(NLF + 1:NEL, NCONT) = DUMMY(NCONT)
         ELSE
            CCAPB(NLF + 1:NEL, NCONT) = DUMMY(NCONT)
         END IF
      END DO

      ! * Number of columns where base concentration is not default value
      CALL ALREDI(0, CMD, CPR, ':CM35', 1, 1, IDUM)
      NCBC = IDUM(1)

      IF (NCBC > 0) THEN
         ! * Numbers and concentrations for those columns
         ! * (read list index as extra column of floating-point data)
         NREQ = (1 + NCONCM)*NCBC
         IF (NREQ > NELEE) THEN
            WRITE (MSG, 9809) NELEE, NREQ, 'non-default columns', 'CM35: NCBC ', NCBC
            CALL RAISE_ERROR(FATAL, 3001, CPR, 0, 0, MSG)
         END IF

         CALL ALREDF(0, CMD, CPR, ':CM37', 1 + NCONCM, NCBC, DUMMY)
         INDX = 1

         DO I = 1, NCBC
            IEL = NINT(DUMMY(INDX))
            IF (IEL <= NLF .OR. IEL > NEL) THEN
               WRITE (MSG, 9811) IEL, 'CM37', 'column element'
               CALL RAISE_ERROR(FATAL, 3002, CPR, 0, 0, MSG)
            END IF

            IF (ISFLXB) THEN
               CALL DCOPY(NCONCM, DUMMY(INDX + 1), 1, CCAPR(IEL, 1), NELEE)
            ELSE
               CALL DCOPY(NCONCM, DUMMY(INDX + 1), 1, CCAPB(IEL, 1), NELEE)
            END IF

            INDX = INDX + 1 + NCONCM
         END DO
      END IF

      ! * Rate of dry deposition, for each contaminant
      CALL ALREDF(0, CMD, CPR, ':CM39', NCONCM, 1, IIICF)

      ! Some Soil Properties
      ! --------------------
      !
      IF (NSCM < 1 .OR. NSCM > NMAX(2)) THEN
         WRITE (MSG, 9819) 'soil types', 'CM21: NSCM', NSCM, NMAX(2)
         CALL RAISE_ERROR(FATAL, 3004, CPR, 0, 0, MSG)
      END IF

      ! * 3 size fractions (used only if SY module inactive)
      ! * (read soil index as extra column of floating-point data)
      CALL ALREDF(0, CMD, CPR, ':CM41', 4, NSCM, DUMMY)
      INDX = 1
      DO I = 1, NSCM
         SOIL = NINT(DUMMY(INDX))
         IF (SOIL < 1 .OR. SOIL > NSCM) THEN
            WRITE (MSG, 9811) SOIL, 'CM41', 'soil type'
            CALL RAISE_ERROR(FATAL, 3006, CPR, 0, 0, MSG)
         END IF
         CALL DCOPY(3, DUMMY(INDX + 1), 1, SOFN(SOIL, 1), NSEE)
         INDX = INDX + 4
      END DO

      ! Some Contaminant Properties
      ! ---------------------------
      !
      ! * Freundlich isotherm power constant
      CALL ALREDF(0, CMD, CPR, ':CM43', NCONCM, 1, GNN)

      ! * Chemical decay constant
      CALL ALREDF(0, CMD, CPR, ':CM45', NCONCM, 1, GGLMSO)

      ! * Coefficients for exchange between bed layers
      CALL ALREDF(0, CMD, CPR, ':CM47', NCONCM, 1, ALPHBD)

      ! * Coefficients for exchange between water and bed
      CALL ALREDF(0, CMD, CPR, ':CM49', NCONCM, 1, ALPHBS)

      ! More Contaminant/Sediment/Soil Properties
      ! -----------------------------------------
      !
      IF (NSEDCM < 1 .OR. NSEDCM > NMAX(3)) THEN
         WRITE (MSG, 9819) 'sediment sizes', 'CM23: NSEDCM', NSEDCM, NMAX(3)
         CALL RAISE_ERROR(FATAL, 3005, CPR, 0, 0, MSG)
      END IF

      ! * Reference Kd for each particle size
      ! * (read contaminant index as extra column of floating-point data)
      CALL ALREDF(0, CMD, CPR, ':CM51', 1 + NSEDCM, NCONCM, DUMMY)
      INDX = 1
      DO I = 1, NCONCM
         NCONT = NINT(DUMMY(INDX))
         IF (NCONT < 1 .OR. NCONT > NCONCM) THEN
            WRITE (MSG, 9811) NCONT, 'CM51', 'contaminant number'
            CALL RAISE_ERROR(FATAL, 3007, CPR, 0, 0, MSG)
         END IF
         CALL DCOPY(NSEDCM, DUMMY(INDX + 1), 1, KDDLS(1, NCONT), 1)
         INDX = INDX + 1 + NSEDCM
      END DO

      ! * Coefficients for exchange between soil regions
      CALL ALREDF(0, CMD, CPR, ':CM53', 1 + NSCM, NCONCM, DUMMY)
      INDX = 1
      DO I = 1, NCONCM
         NCONT = NINT(DUMMY(INDX))
         IF (NCONT < 1 .OR. NCONT > NCONCM) THEN
            WRITE (MSG, 9811) NCONT, 'CM53', 'contaminant number'
            CALL RAISE_ERROR(FATAL, 3007, CPR, 0, 0, MSG)
         END IF
         CALL DCOPY(NSCM, DUMMY(INDX + 1), 1, ALPHA(1, NCONT), 1)
         INDX = INDX + 1 + NSCM
      END DO

      ! * Fraction of adsorption sites in dynamic region
      CALL ALREDF(0, CMD, CPR, ':CM55', 1 + NSCM, NCONCM, DUMMY)
      INDX = 1
      DO I = 1, NCONCM
         NCONT = NINT(DUMMY(INDX))
         IF (NCONT < 1 .OR. NCONT > NCONCM) THEN
            WRITE (MSG, 9811) NCONT, 'CM55', 'contaminant number'
            CALL RAISE_ERROR(FATAL, 3007, CPR, 0, 0, MSG)
         END IF
         CALL DCOPY(NSCM, DUMMY(INDX + 1), 1, FADS(1, NCONT), 1)
         INDX = INDX + 1 + NSCM
      END DO

      ! * Fraction of pore water in dynamic region
      CALL ALREDF(0, CMD, CPR, ':CM57', NSCM, 1, PHIDAT)

      ! * Diffusion coefficient
      CALL ALREDF(0, CMD, CPR, ':CM59', NCONCM, 1, DIFDAT)

      ! * Dispersivity
      CALL ALREDF(0, CMD, CPR, ':CM61', 1 + NSCM, NCONCM, DUMMY)
      INDX = 1
      DO I = 1, NCONCM
         NCONT = NINT(DUMMY(INDX))
         IF (NCONT < 1 .OR. NCONT > NCONCM) THEN
            WRITE (MSG, 9811) NCONT, 'CM61', 'contaminant number'
            CALL RAISE_ERROR(FATAL, 3007, CPR, 0, 0, MSG)
         END IF
         CALL DCOPY(NSCM, DUMMY(INDX + 1), 1, DISPDT(1, NCONT), 1)
         INDX = INDX + 1 + NSCM
      END DO

      ! Epilogue
      ! -----------
      !
      ! * Close the data file
      CALL ALRED2(1, CMD, CPR, 'CMD')

      ! * Is everything defined?
      IF (NCONCM < NCON .OR. NSCM < NS .OR. NSEDCM < NSED) THEN
         WRITE (MSG, 9800) NCONCM, NSCM, NSEDCM, NCON, NS, NSED
         CALL RAISE_ERROR(FATAL, 3008, CPR, 0, 0, MSG)
      END IF

      RETURN

      ! Formats
      ! -------
      !
9800  FORMAT('No. of contaminants/soils/sediments with data', &
             ' (CM19-23: NCONCM/NSCM/NSEDCM = ', 2(I3, '/'), I3, ')', &
             ' must be at least ', 2(I3, '/'), I3)

9809  FORMAT('Insufficient workspace (have NELEE =', I6, ', need', I6, ')', &
             ' for the number of ', A, ' given (', A, ' =', I6, ')')

9811  FORMAT('Index', I6, ' (given as part of data item ', A, ')', &
             ' is not a valid ', A)

9819  FORMAT('Number of ', A, ' with data (', A, ' =', I6, ')', &
             ' must be positive & not greater than', I6)

   END SUBROUTINE CMRD

!> @brief Initialises the contaminant component and contaminant interface arrays.
!>
!> The routine reads contaminant data via [[cm_input:CMRD]], checks tabulated
!> spatially variable concentrations, builds column/link geometry terms, sets
!> contaminant storage coefficients, interpolates initial column concentrations,
!> and initialises plant uptake data when enabled.
!>
!> | Phase | Main state prepared |
!> |:------|:--------------------|
!> | Input and checking | `CMRD` reads CM/CMP data; [[muerr2]] checks spatial concentration tables. |
!> | Sediment interface | If `ISSDON` is false, neutral three-fraction sediment state is generated for contaminant coupling. |
!> | Scaling and coefficients | Contaminant scaling constants, decay coefficients, and soil `KDDSOL` values are set. |
!> | Column/link geometry | Column bottoms, lateral overlaps, bank/link bed layers, and stream-bed storage areas are derived. |
!> | Old-state initialisation | Link, column, surface-flow, vertical-flow, moisture, and concentration old-state arrays are copied from the current hydraulic state. |
!> | Optional spatial concentration | `ALINTP` maps depth-concentration tables onto active column cells. |
!>
!> `INCM` sets contaminant scaling constants before any solve-time coefficients
!> are assembled:
!>
!> \[
!> Z2 = 50,\qquad D0 = 10^{-3},\qquad OODO=1/D0,
!> \]
!>
!> \[
!> Z2SQ=Z2^2,\qquad Z2OD=Z2/D0,\qquad Z2SQOD=Z2^2/D0.
!> \]
!>
!> The finite-difference weighting is initialised as fully implicit through
!> `SGMA=1`, `SGSQ=SGMA**2`, and `OMSGMA=1-SGMA`. Contaminant decay is scaled
!> for the solver as
!>
!> \[
!> GCPLA_c = GGLMSO_c\,Z2SQOD.
!> \]
!>
!> For each soil type and contaminant, the soil reference distribution
!> coefficient is reconstructed from sediment particle fractions and
!> particle-size distribution coefficients:
!>
!> \[
!> KDDSOL_{s,c} = \sum_j SOSDFN_{s,j}\,KDDLS_{j,c}.
!> \]
!>
!> If the sediment component is inactive, `INCM` creates a neutral sediment
!> interface: three sediment fractions, no loose/deposited sediment mass, first
!> fraction equal to one, zero sediment fluxes, and bed soil/porosity inferred
!> from the bank soil at the exposed channel bed. This gives the contaminant
!> component consistent sediment arrays without running sediment transport.
!>
!> Column geometry is prepared from VSS layering. `NCOLMB` is set to each
!> column's bottom active layer, `ZCOLMB` stores the corresponding node
!> elevation, and the scaled cell thickness workspace is
!>
!> \[
!> KSPDUM_{e,k}=DELTAZ_{k,e}/Z2.
!> \]
!>
!> Lateral overlap arrays `NOL`, `NOLBT`, `NOLCE`, `NOLCEA`, and `JOLFN` are
!> built from `JVSACN`, `JVSDEL`, and `DELTAZ`. Where an overlap spans two
!> cells, `JOLFN` stores the fractional contribution on the legacy integer scale
!> 32500, for example
!>
!> \[
!> JOLFN =
!> \left\lfloor
!> 32500\,\frac{DELTAZ_k}{DELTAZ_k+DELTAZ_{k+1}}
!> \right\rfloor .
!> \]
!>
!> For each channel link, the routine derives the bed-deep cell numbers and
!> fractional coverage (`NCEBD`, `FNCEBD`) on both adjacent banks from the
!> specified deep-bed thickness `DBDI/Z2` and reconciles the two bank overlap
!> systems so all bank soil below the channel is accounted for. It then sets the
!> bed-surface and bed-deep storage coefficients:
!>
!> \[
!> ACPBSG_l = DBS\,CWIDTH_l/Z2^2,
!> \]
!>
!> \[
!> ACPBI_l =
!> \frac{1}{2}\left(\sum \Delta z^\*_{bank}\right)CWIDTH_l/Z2
!> - ACPBSG_l,
!> \]
!>
!> where the summed scaled bank thickness excludes the parts outside the
!> bed-surface/deep-bed region.
!>
!> Link initial concentrations are set to the incoming concentration `CCAPIN`
!> in the deep-bed, bed-surface, and stream-water cells. Initial stream-bed
!> moisture is the thickness-weighted average over the two adjacent bank regions
!> participating in the bed layers, capped by bed porosity:
!>
!> \[
!> THBED_l =
!> \min\left(PBSED_l,\frac{\sum_k VSTHE_k w_k}{\sum_k w_k}\right).
!> \]
!>
!> Initial bed particle fractions combine loose sediment and parent bed
!> material:
!>
!> \[
!> FBBEDO_{l,j} =
!> \frac{DLS_l\,CWIDTH_l\,FBETA_{l,j}
!>       +(ACPBI_l-ACPBSG_l)Z2^2\,SOSDFN_{NSOBED_l,j}}
!>      {DLS_l\,CWIDTH_l +(ACPBI_l-ACPBSG_l)Z2^2}.
!> \]
!>
!> @note
!> If `NSOBED(l)` is zero during this calculation, the current code sets it to
!> soil type 1 before using `SOSDFN`. The in-line comment identifies this as a
!> temporary fix for cases where sediment and solute components run together.
!> @endnote
!>
!> For soil and bank columns, old-state flow and concentration arrays are
!> initialised from current water-flow state. Surface input and bottom flux use
!>
!> \[
!> QIO_e=-PNETTO_e\,cellarea_e,\qquad
!> QQRFO_e=QVSV_{NCOLMB(e),e}\,cellarea_e,
!> \]
!>
!> and surface-water depth is stored as `DSWO = HRF - ZGRUND`. Bank columns use
!> an L-shaped correction factor
!>
!> \[
!> \rho = \frac{cellarea_{bank}/CLENTH_l}
!>             {cellarea_{bank}/CLENTH_l + 0.5\,CWIDTH_l},
!> \]
!>
!> to blend bank and associated-link water contents and vertical velocities
!> where the contaminant column represents both bank soil and channel-underflow
!> geometry.
!>
!> Surface-flow old-state values are converted to the contaminant component's
!> inward-positive convention as
!>
!> \[
!> QQQSWO_{e,1:2}=-QOC_{e,1:2},\qquad
!> QQQSWO_{e,3:4}= QOC_{e,3:4}.
!> \]
!>
!> If `CMRD` marked an initial concentration as spatially variable, `INCM`
!> calls `ALINTP` to interpolate the category-specific concentration/depth table
!> onto every active column cell and copies the result into both current and old
!> mobile/dead-space concentration arrays (`CCCC`, `SSSS`, `CCCCO`, `SSSSO`).
!> Finally, plant uptake data are initialised through [[inpl]] when `ISPLT` is
!> enabled.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Standardised declarations. |
!> | 1996-1998 | GP/RAH | 4.0-4.2 | Reworked VSS coupling, overlap geometry, sediment interfaces, and explicit typing. |
!> | 2026-03 | SB | 4.6 | Updated contaminant allocation and active-cell interpolation. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE INCM(ISSDON)


      IMPLICIT NONE

      ! Input arguments
      LOGICAL, INTENT(IN) :: ISSDON

      ! Locals, etc
      INTEGER :: ICL, IDEL, IEL, IFA, ITYPE, ITYPEA
      INTEGER :: JA, JAL, JBK, JBKU, JCL, JDEL, JDUM, JEL, JFA, JFLINK
      INTEGER :: JLYR, JSED, JSOIL, LDUM
      INTEGER :: NBKU, NCDUM, NCE, NCE1, NCE2, NCEA, NCL, NCONT
      INTEGER :: NDIFF, NDUM, NDUMA, NELMA, NLINK, NLINKA, NLINKU
      INTEGER :: NOL1, NOL2, NOLBD, NOLDUM, NOLP, NOLX
      INTEGER :: JFCE(2), JOLDUM(2), NBK(2), NCEDUM(2)
      DOUBLE PRECISION :: ARL, ARP, DBK, DKBED, DMULT, DUM, DUM1, DUM2, DUM3, DUMK
      DOUBLE PRECISION :: FNOLBD, asum, asumK

      DOUBLE PRECISION :: FNDUM(2), FOLDUM(2), ROH(LLEE)
      DOUBLE PRECISION, ALLOCATABLE :: KSPDUM(:, :)

      ! Added by SB
      INTEGER :: MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS
      INTEGER :: NUM_CATEGORIES_TYPES(NCONEE), NTAB(NOCTAB, NCONEE)
      INTEGER, ALLOCATABLE :: NCATTY(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: TABLE_CONCENTRATION(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: TABLE_WATER_DEPTH(:, :, :)
      DOUBLE PRECISION, ALLOCATABLE :: DUMMYCONC(:, :)

      LOGICAL :: LDUM1(1), ISCNSV(NCONEE)

      INTEGER(KIND=I_P):: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "FRmod:INCM"

      ! New by SB 18/11/04
      ! contam.f removed. z2 and d0 (scaling variables) needed here
      Z2 = 50.0D0
      D0 = 1.0D-3

      ! New by SB
      ! Parameter values for spatially variable initial contaminant conc.
      !
      MAX_NUM_CATEGORY_TYPES = NOCTAB
      MAX_NUM_DATA_PAIRS = NOCTAB

      ALLOCATE (KSPDUM(total_no_elements, top_cell_no + 1), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "KSPDUM", location, emsg)
      ALLOCATE (DUMMYCONC(total_no_elements, top_cell_no), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "DUMMYCONC", location, emsg)
      ALLOCATE (NCATTY(NELEE, NCONEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "NCATTY", location, emsg)
      ALLOCATE (TABLE_CONCENTRATION(NOCTAB, NOCTAB, NCONEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TABLE_CONCENTRATION", location, emsg)
      ALLOCATE (TABLE_WATER_DEPTH(NOCTAB, NOCTAB, NCONEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "TABLE_WATER_DEPTH", location, emsg)

      ! Read main CM input data file
      ! Modified by SB

      CALL CMRD(CMD, CMP, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, total_no_elements, total_no_links, NLFEE, NSEE, &
                NS, NSEDEE, NSED, MAX_NUM_DATA_PAIRS, NX, NXEE, NYEE, NY, NLYRBT(total_no_links + 1, 1), &
                ICMXY, ICMBK, ICMREF(1, 5), BEXBK, LINKNS, NUM_CATEGORIES_TYPES, NCATTY, NCON, &
                NCOLMB(total_no_links + 1), NTAB, DBS, DBDI, CCAPI, CCAPE, CCAPR, CCAPB, &
                TABLE_CONCENTRATION, TABLE_WATER_DEPTH, IIICF, SOFN, GNN, GGLMSO, ALPHBD, ALPHBS, KDDLS, &
                ALPHA, FADS, ISCNSV, IDUM, DUMMY)
      ! Checks the data used to calculate spatially variable
      ! concentrations in the grid and bank elements is OK

      CALL MUERR2(CMP, total_no_elements, NELEE, total_no_links, MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCON, NCONEE, &
                  NUM_CATEGORIES_TYPES, NTAB, NCATTY, ISCNSV, TABLE_CONCENTRATION, TABLE_WATER_DEPTH, LDUM1)

      DO NCL = total_no_links + 1, total_no_elements
         NCOLMB(NCL) = NLYRBT(NCL, 1)
      END DO

      IF (.NOT. ISSDON) THEN
         ! ssssss INITIALISE SEDIMENT VARIABLES sssss
         NSED = 3
         DO NLINK = 1, total_no_links
            ARBDEP(NLINK) = zero
            DLS(NLINK) = zero
            DLSO(NLINK) = zero

            FBETA(NLINK, 1:3) = [one, zero, zero]
            FBTSD(NLINK, 1:3) = [one, zero, zero]
            FDEL(NLINK, 1:3) = [zero, zero, zero]
            GINFD(NLINK, 1:3) = [zero, zero, zero]
            GINFS(NLINK, 1:3) = [zero, zero, zero]

            GNUBK(NLINK) = zero
            QDEFF(NLINK, 1:2) = zero

            DO JA = 1, 4
               NELMA = ICMREF(NLINK, JA + 4)
               IF (NELMA > 0) THEN
                  ITYPEA = ICMREF(NELMA, 1)
                  IF (ITYPEA == 1) THEN
                     NBK(1) = NELMA
                  ELSE IF (ITYPEA == 2) THEN
                     NBK(2) = NELMA
                  END IF
               END IF
            END DO

            JLYR = 0
            search_lyr_loop: DO
               JLYR = JLYR + 1
               IF (NLYRBT(NBK(1), JLYR) >= NHBED(NLINK, 1)) EXIT search_lyr_loop
            END DO search_lyr_loop

            NSOBED(NLINK) = NTSOIL(NBK(1), JLYR - 1)
            PBSED(NLINK) = VSPOR(NSOBED(NLINK))
            ! SET BED SOIL TYPE AND POROSITY, BASED ON THE SOIL AT THE
            ! BOTTOM OF THE EXPOSED FACE OF BANK 1
         END DO

         DO NCL = total_no_links + 1, total_no_elements
            DLS(NCL) = zero
            DLSO(NCL) = zero
            FDEL(NCL, 1:3) = [zero, zero, zero]
            FBETA(NCL, 1:3) = [one, zero, zero]
            GNU(NCL) = zero
            GNUO(NCL) = zero
         END DO

         DO JSOIL = 1, NSEE
            SOSDFN(JSOIL, 1:3) = SOFN(JSOIL, 1:3)
         END DO
         ! SET SEDIMENT FRACTIONS FOR SOIL TYPES

      END IF
      ! IF THE SEDIMENT CODE IS NOT ACTIVE, THE SEDIMENT VARIABLES ARE SET TO APPROPRIATE VALUES
      ! ccccccccccccc SET CONSTANTS cccccccccccccc

      SCL = one/32500.0D0
      OODO = one/D0

      ! SCALING FACTORS
      Z2SQ = Z2*Z2
      Z2OD = OODO*Z2
      Z2SQOD = OODO*Z2SQ

      ! SCALING VALUES
      SGMA = one
      SGSQ = SGMA*SGMA
      OMSGMA = one - SGMA

      ! FINITE DIFFERENCE IMPLICIT WEIGHTING
      NCETOP = top_cell_no

      DO NCONT = 1, NCON
         ! SET CONSTANTS WHICH DEPEND ON CONTAMINANT NUMBER
         GCPLA(NCONT) = GGLMSO(NCONT)*Z2SQOD
         ! SET DECAY CONSTANTS FOR CONTAMINANTS

         DO JSOIL = 1, NS
            asum = SUM(SOSDFN(JSOIL, 1:NSED)*KDDLS(1:NSED, NCONT))
            KDDSOL(JSOIL, NCONT) = asum
         END DO
         ! SET REFERENCE DISTRIBUTION COEFFICIENT FOR SOIL TO MATCH THAT SPECIFIED FOR THE
         ! SEDIMENT PARTICLE SIZE GROUPS
      END DO

      DO NCL = total_no_links + 1, total_no_elements
         ZCOLMB(NCL) = ZVSNOD(NCOLMB(NCL), NCL)
      END DO
      ! SET ELEVATION OF BOTTOM CELLS IN SOIL COLUMNS

      ! set up temporary array for use until full vss coding completed
      DO NCL = 1, total_no_elements
         DO NCE = NLYRBT(NCL, 1), top_cell_no
            KSPDUM(NCL, NCE) = DELTAZ(NCE, NCL)/Z2
         END DO
         KSPDUM(NCL, top_cell_no + 1) = KSPDUM(NCL, top_cell_no)
      END DO

      ! Set up NOL, NOLBT, NOLCE, NOLCEA, JOLFN using VSS arrays JVSACN,
      ! JVSDEL and DELTAZ
      ! NB. NOLBT and JOLFN are overwritten during the loop over a column

      DO IEL = total_no_links + 1, total_no_elements
         DO IFA = 1, 4
            JEL = ICMREF(IEL, IFA + 4)
            JFA = ICMREF(IEL, IFA + 8)
            IF (JEL == 0) THEN
               JEL = IEL
               JFA = IFA
            ELSE IF (ICMREF(JEL, 1) == 3) THEN
               JEL = ICMREF(JEL, IFA + 4)
            END IF

            NOLP = 0
            DO ICL = NLYRBT(IEL, 1), top_cell_no
               IF (JVSACN(IFA, ICL, IEL) > 0) THEN
                  JCL = JVSACN(IFA, ICL, IEL)
                  IDEL = JVSDEL(IFA, ICL, IEL)
                  JDEL = JVSDEL(JFA, JCL, JEL)

                  NOLP = NOLP + 1
                  NOLCE(IEL, NOLP, IFA) = ICL
                  NOLCEA(IEL, NOLP, IFA) = JCL
                  NOLBT(IEL, ICL, IFA) = NOLP

                  IF (IDEL == 1) THEN
                     JOLFN(IEL, NOLP, IFA) = INT(32500.0D0*DELTAZ(ICL, IEL)/(DELTAZ(ICL, IEL) + DELTAZ(ICL + 1, IEL)))
                     NOLP = NOLP + 1
                     NOLCE(IEL, NOLP, IFA) = ICL + 1
                     NOLCEA(IEL, NOLP, IFA) = JCL
                     JOLFN(IEL, NOLP, IFA) = INT(32500.0D0*DELTAZ(ICL + 1, IEL)/(DELTAZ(ICL, IEL) + DELTAZ(ICL + 1, IEL)))
                  ELSE IF (JDEL == 1) THEN
                     NOLP = NOLP + 1
                     NOLCE(IEL, NOLP, IFA) = ICL
                     NOLCEA(IEL, NOLP, IFA) = JCL + 1
                  ELSE
                     JOLFN(IEL, NOLP, IFA) = 32500
                  END IF
               END IF
            END DO

            NOL(IEL, IFA) = NOLP
            NOLBT(IEL, top_cell_no + 1, IFA) = NOLP + 1
         END DO
      END DO

      DKBED = DBDI/Z2
      DO NLINK = 1, total_no_links
         ! ^^^^^^^^^ SET CONSTANTS FOR LINKS ^^^^^^^^
         DO JA = 1, 4
            NDUMA = ICMREF(NLINK, JA + 4)
            IF (NDUMA > 0) THEN
               ITYPEA = ICMREF(NDUMA, 1)
               IF (ITYPEA == 1 .OR. ITYPEA == 2) THEN
                  ! ADJACENT ELEMENT IS A BANK
                  JBK = ITYPEA
                  NBK(JBK) = NDUMA
                  ! USED ONLY IN THIS ROUTINE
                  NBANK(NLINK, JBK) = NDUMA
                  ! SAVED FOR USE IN OTHER SUBROUTINES

                  asum = FHBED(NLINK, JBK)*KSPDUM(NBK(JBK), NHBED(NLINK, JBK) + 1)
                  IF (asum >= DKBED) THEN
                     NCEDUM(JBK) = NHBED(NLINK, JBK)
                     FNDUM(JBK) = (asum - DKBED)/KSPDUM(NBK(JBK), NHBED(NLINK, JBK) + 1)
                  ELSE
                     NCE = NHBED(NLINK, JBK)

                     bed_depth_loop: DO
                        NCE = NCE - 1
                        asum = asum + KSPDUM(NBK(JBK), NCE + 1)
                        IF (asum > DKBED) EXIT bed_depth_loop
                     END DO bed_depth_loop

                     NCEDUM(JBK) = NCE
                     FNDUM(JBK) = (asum - DKBED)/KSPDUM(NBK(JBK), NCE + 1)
                  END IF

                  ! NCEDUM AND FNDUM ARE THE 1ST ESTIMATES FOR NCEBD AND FNCEBD.
                  ! THEY ARE THE CORRECT VALUES FOR A TOTAL BED THICKNESS OF DBDI METRES.
                  ! CHANGES ARE MADE LATER SO THAT A SINGLE OVERLAP NUMBER AND FRACTION
                  ! (NOLBD AND FNOLBD) CAN BE ASSOCIATED WITH THE REGION BELOW THE DEEP BED.

                  asum = zero
                  JFCE(JBK) = JA + SIGN(2, 2 - JA)
                  NOLP = NOLBT(NBK(JBK), NCEDUM(JBK) + 1, JFCE(JBK)) - 1

                  fraction_loop: DO
                     NOLP = NOLP + 1
                     DUM1 = SCL*JOLFN(NBK(JBK), NOLP, JFCE(JBK))
                     asum = asum + DUM1
                     IF (asum > FNDUM(JBK)) EXIT fraction_loop
                  END DO fraction_loop

                  JOLDUM(JBK) = NOLP - 1
                  FOLDUM(JBK) = (FNDUM(JBK) - asum + DUM1)/DUM1
                  ! OVERLAP NUMBERS AND FRACTIONS ASSOCIATED WITH THE 1ST ESTIMATES
               END IF
            END IF
         END DO

         DUM1 = DBLE(JOLDUM(1)) + FNDUM(1)
         DUM2 = DBLE(JOLDUM(2)) + FNDUM(2)
         IF (DUM1 <= DUM2) THEN
            NOLBD = JOLDUM(1)
            FNOLBD = FNDUM(1)
            NCEBD(NLINK, 1) = NCEDUM(1)
            FNCEBD(NLINK, 1) = FNDUM(1)
            LDUM = 2
         ELSE
            NOLBD = JOLDUM(2)
            FNOLBD = FNDUM(2)
            NCEBD(NLINK, 2) = NCEDUM(2)
            FNCEBD(NLINK, 2) = FNDUM(2)
            LDUM = 1
         END IF

         NCDUM = NOLCE(NBK(LDUM), NOLBD, JFCE(LDUM))
         NOLDUM = NOLBT(NBK(LDUM), NCDUM + 1, JFCE(LDUM)) - 1
         ! HIGHEST OVERLAP ASSOC. WITH NCDUM

         DUM3 = FNOLBD*SCL*DBLE(JOLFN(NBK(LDUM), NOLBD + 1, JFCE(LDUM)))
         ! FRACTION OF NEXT HIGHEST CELL COVERED BY FRACTION OF OVERLAP

         IF (NOLDUM == NOLBD) THEN
            NCEBD(NLINK, LDUM) = NCDUM
            FNCEBD(NLINK, LDUM) = DUM3
         ELSE
            NCEBD(NLINK, LDUM) = NCDUM - 1
            asum = DUM3
            DO NOLP = NOLBT(NBK(LDUM), NCDUM, JFCE(LDUM)), NOLBD
               asum = asum + SCL*DBLE(JOLFN(NBK(LDUM), NOLP, JFCE(LDUM)))
            END DO
            FNCEBD(NLINK, LDUM) = asum
         END IF
         ! SET FINAL VALUES FOR THE OVERLAP NUMBERS NOLBD AND FRACTIONS FNOLBD
         ! FOR THE REGION BELOW THE DEEP BED; AND SET THE CELL NUMBERS NCEBD
         ! AND FRACTIONS FNCEBD ACCORDINGLY

         asum = zero
         DO JBK = 1, 2
            DO NCE = NCEBD(NLINK, JBK) + 1, NHBED(NLINK, JBK) + 1
               asum = asum + KSPDUM(NBK(JBK), NCE)
            END DO
            asum = asum - FNCEBD(NLINK, JBK)*KSPDUM(NBK(JBK), NCEBD(NLINK, JBK) + 1)
            asum = asum - (one - FHBED(NLINK, JBK))*KSPDUM(NBK(JBK), NHBED(NLINK, JBK) + 1)
         END DO

         ACPBSG(NLINK) = DBS*CWIDTH(NLINK)/Z2SQ
         ACPBI(NLINK) = (half*asum*CWIDTH(NLINK)/Z2) - ACPBSG(NLINK)
         ! SET BED SURFACE LAYER THICKNESS TO DBS METRES, AND THE COMBINED AREA OF THE
         ! BED SURFACE AND DEEP LAYERS TO THE AREA ABOVE OVERLAP NOLBD AND FRACTION FNOLBD

         DO JBK = 1, 2
            ! uuuuuuu ADJUST TRANSMISIVITIES FOR uuuuuuu
            ! UPSTREAM AND DOWNSTREAM SUBSURFACE FLOW IN BANKS
            NCE1 = NHBED(NLINK, JBK)
            DO JA = 1, 4
               NDUMA = ICMREF(NBK(JBK), JA + 4)
               IF (NDUMA /= 0) THEN
                  ITYPEA = ICMREF(NDUMA, 1)
                  IF (ITYPEA == 1 .OR. ITYPEA == 2) THEN
                     ! THE ELEMENT UPSTREAM OR DOWNSTREAM FROM BANK JBK OF LINK NLINK IS ITSELF A BANK
                     NOL1 = NOLBT(NBK(JBK), NCE1 + 1, JA) - 1
                     NBKU = NDUMA
                     NLINKU = ICMREF(NBKU, 4)

                     IF (ICMBK(NLINKU, 1) == NBKU) THEN
                        JBKU = 1
                     ELSE
                        JBKU = 2
                     END IF

                     NCE2 = NHBED(NLINKU, JBKU)
                     NOL2 = NOLBT(NBKU, NCE2 + 1, ICMREF(NBK(JBK), JA + 8)) - 1
                     ! USE ICMREF SO CORRECT FACE IS FOUND EVEN IF THE UPSTREAM OR DOWNSTREAM BANK IS ROUND A CORNER

                     NOLX = MIN(NOL1, NOL2)
                     DUM1 = cellarea(NBK(JBK))/CLENTH(NLINK) + cellarea(NBKU)/CLENTH(NLINKU)
                     DUM2 = half*(cellarea(NLINK)/CLENTH(NLINK) + cellarea(NLINKU)/CLENTH(NLINKU))
                     DMULT = DUM1/(DUM1 + DUM2)

                     DO NOLP = NOLX + 1, NOL(NBK(JBK), JA)
                        JKZCOL(NBK(JBK), NOLP, JA) = MAX(1, INT(DMULT*JKZCOL(NBK(JBK), NOLP, JA)))
                     END DO
                  END IF
               END IF
            END DO
         END DO

         DO JBK = 1, 2
            NCEAB(NLINK, JBK) = NHBED(NLINK, JBK)
         END DO

      END DO

      DO NCONT = 1, NCON
         ! xxxxxxx INITIALISE VARIABLES WHICH DEPEND ON CONTAMINANT NUMBER xxxxxxx
         CCAPIO(NCONT) = CCAPI(NCONT)
         IIICFO(NCONT) = IIICF(NCONT)
      END DO

      DO NLINK = 1, total_no_links
         ! ooooooo INITIALISE LINK VARIABLES oooooooo
         ACPSFO(NLINK) = ARXL(NLINK)/Z2SQ
         ACPBDO(NLINK) = ACPBI(NLINK)

         DO NCONT = 1, NCON
            CCCCO(NLINK, NCETOP - 2:NCETOP, NCONT) = CCAPIN(NCONT)
            CCCC(NLINK, NCETOP - 2:NCETOP, NCONT) = CCAPIN(NCONT)
         END DO

         asumK = zero
         asum = zero
         DO JBK = 1, 2
            NDUM = NCEBD(NLINK, JBK) + 1
            NCE = NDUM
            DUMK = (one - FNCEBD(NLINK, JBK))*KSPDUM(ICMBK(NLINK, JBK), NCE)
            asumK = asumK + DUMK
            asum = asum + VSTHE(NCE, NBK(JBK))*DUMK

            DO NCE = NDUM + 1, NHBED(NLINK, JBK)
               DUMK = KSPDUM(ICMBK(NLINK, JBK), NCE)
               asumK = asumK + DUMK
               asum = asum + VSTHE(NCE, NBK(JBK))*DUMK
            END DO

            NCE = NHBED(NLINK, JBK) + 1
            DUMK = FHBED(NLINK, JBK)*KSPDUM(ICMBK(NLINK, JBK), NCE)
            asumK = asumK + DUMK
            asum = asum + VSTHE(NCE, NBK(JBK))*DUMK
         END DO

         THBEDO(NLINK) = MIN(PBSED(NLINK), asum/asumK)
         THBED(NLINK) = THBEDO(NLINK)

         ARL = DLS(NLINK)*CWIDTH(NLINK)
         ARP = (ACPBI(NLINK) - ACPBSG(NLINK))*Z2SQ
         DUM = one/(ARL + ARP)

         DO JSED = 1, NSED
            ! sb temp fix 09022026: NSOBED fallback
            IF (NSOBED(NLINK) == 0) NSOBED(NLINK) = 1

            FBBEDO(NLINK, JSED) = DUM*(ARL*FBETA(NLINK, JSED) + ARP*SOSDFN(NSOBED(NLINK), JSED))
            FDELO(NLINK, JSED) = FDEL(NLINK, JSED)
            FBTSDO(NLINK, JSED) = FBTSD(NLINK, JSED)
         END DO
      END DO

      DO NCL = total_no_links + 1, total_no_elements
         ! iiiiii INITIALISE COLUMN VARIABLES iiiiiii
         DLSO(NCL) = DLS(NCL)
         DSWO(NCL) = GETHRF(NCL) - ZGRUND(NCL)
         GNUO(NCL) = GNU(NCL)
         QIO(NCL) = -PNETTO(NCL)*cellarea(NCL)
         QQRFO(NCL) = QVSV(NCOLMB(NCL), NCL)*cellarea(NCL)
         RSZWLO(NCL) = zero
         ZONEO(NCL) = (ZGRUND(NCL) - ZCOLMB(NCL))/Z2

         DO JDUM = 1, 2
            QQQSWO(NCL, JDUM) = -QOC(NCL, JDUM)
            QQQSWO(NCL, JDUM + 2) = QOC(NCL, JDUM + 2)
         END DO

         ! set up variables for l-shaped bank calculations, if required
         ITYPE = ICMREF(NCL, 1)
         IF (ITYPE /= 0) THEN
            JBK = ITYPE
            NLINKA = ICMREF(NCL, 4)
            JAL = 0

            link_face_loop: DO
               JAL = JAL + 1
               IF (ICMREF(NLINKA, JAL + 4) == NCL) EXIT link_face_loop
            END DO link_face_loop

            JFLINK = ICMREF(NLINKA, JAL + 8)
            DBK = cellarea(NCL)/CLENTH(NLINKA)
            DMULT = DBK/(DBK + half*CWIDTH(NLINKA))

            DO NCE = NLYRBT(NCL, 1) - 1, NCEBD(NLINKA, JBK)
               ROH(NCE) = DMULT
            END DO

            NCE = NCEBD(NLINKA, JBK) + 1
            ROH(NCE) = one - (one - DMULT)*FNCEBD(NLINKA, JBK)

            DO NCE = NCEBD(NLINKA, JBK) + 2, LLEE
               ROH(NCE) = one
            END DO
         END IF

         DO NCE = 1, top_cell_no  !LLEE  !JE
            GGAMMO(NCL, NCE) = zero
            DO JA = 1, 4
               QQO(NCL, NCE, JA) = QVSH(JA, NCE, NCL)
            END DO

            DO NCONT = 1, NCON
               CCCCO(NCL, NCE, NCONT) = CCAPIN(NCONT)
               SSSSO(NCL, NCE, NCONT) = CCAPIN(NCONT)
               CCCC(NCL, NCE, NCONT) = CCAPIN(NCONT)
               SSSS(NCL, NCE, NCONT) = CCAPIN(NCONT)
            END DO
         END DO

         ! calculate theta and vert vel for L-shaped bank, if required
         IF (ITYPE == 0) THEN
            DO NCE = NLYRBT(NCL, 1) - 1, top_cell_no
               VSTHEO(NCL, NCE) = VSTHE(NCE, NCL)
               UUAJPO(NCL, NCE) = QVSV(NCE, NCL)
            END DO
         ELSE
            NDIFF = NLYRBT(NLINKA, 1) - NLYRBT(NCL, 1)
            DO NCE = NLYRBT(NCL, 1) - 1, top_cell_no
               NCEA = NCE + NDIFF
               IF (NCEA <= top_cell_no) THEN
                  VSTHEO(NCL, NCE) = ((one - ROH(NCE))*VSTHE(NCEA, NLINKA) + ROH(NCE)*VSTHE(NCE, NCL))
                  UUAJPO(NCL, NCE) = ((one - ROH(NCE))*QVSV(NCEA, NLINKA) + ROH(NCE)*QVSV(NCE, NCL))/ROH(NCE)
               ELSE
                  VSTHEO(NCL, NCE) = VSTHE(NCE, NCL)
                  UUAJPO(NCL, NCE) = QVSV(NCE, NCL)
               END IF
            END DO
         END IF

      END DO

      ! New code by SB --------------
      DO NCONT = 1, NCON
         IF (ISCNSV(NCONT)) THEN
            CALL ALINTP(LLEE, NCETOP, total_no_elements, NELEE, total_no_links, NUM_CATEGORIES_TYPES(NCONT), &
                        MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCATTY(total_no_links + 1, NCONT), NCOLMB(total_no_links + 1), &
                        NTAB(1, NCONT), TABLE_CONCENTRATION(1, 1, NCONT), TABLE_WATER_DEPTH(1, 1, NCONT), &
                        DELTAZ, ZVSNOD, DUMMYCONC)

            DO NCL = total_no_links + 1, total_no_elements
               DO NCE = NCOLMB(NCL), NCETOP
                  CCCC(NCL, NCE, NCONT) = DUMMYCONC(NCL, NCE)
                  SSSS(NCL, NCE, NCONT) = CCCC(NCL, NCE, NCONT)
                  ! ADDED SB 6/3/00
                  SSSSO(NCL, NCE, NCONT) = CCCC(NCL, NCE, NCONT)
                  CCCCO(NCL, NCE, NCONT) = CCCC(NCL, NCE, NCONT)
               END DO
            END DO
         END IF
      END DO
      ! End of new code by SB -------

      IF (ISPLT) CALL INPL

   END SUBROUTINE INCM

!> @brief Checks spatially variable contaminant concentration tables.
!>
!> `MUERR2` verifies that category counts, table lengths, water-depth breakpoints,
!> and concentration values are valid before the contaminant initialisation uses
!> them to interpolate grid and bank concentrations.
!>
!> Checks are applied only for contaminants whose `ISCNSV` flag is true:
!>
!> | Data checked | Condition |
!> |:-------------|:----------|
!> | `NCATTY(J,I)` for non-link elements `J=total_no_links+1:total_no_elements` | Category type must be greater than zero. |
!> | `TABLE_WATER_DEPTH(NELMTY,1,I)` | First depth breakpoint must equal zero. |
!> | `TABLE_WATER_DEPTH(NELMTY,NTBL,I)`, `NTBL>=2` | Depth breakpoints must strictly increase. |
!> | `TABLE_CONCENTRATION(NELMTY,NTBL,I)` | Concentrations must be non-negative. |
!>
!> Errors are accumulated through `ALCHKI`/`ALCHK` into `NERR`; any positive
!> count triggers fatal error 2107 at the end of the routine.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | Legacy | - | 4.2 | Added validation of spatially variable contaminant concentration tables. |
!> | 2026-04-13 | SvB | 4.6.1 | Retained the checker during structured-control-flow conversion. |
!> @endhistory
   SUBROUTINE MUERR2(CPR, total_no_elements, NELEE, total_no_links, MAX_NUM_CATEGORY_TYPES, &
                     MAX_NUM_DATA_PAIRS, NCON, NCONEE, NUM_CATEGORIES_TYPES, NTAB, NCATTY, &
                     ISCNSV, TABLE_CONCENTRATION, TABLE_WATER_DEPTH, LDUM)

      IMPLICIT NONE

      ! --- Dummy Arguments ---
      INTEGER, INTENT(IN) :: CPR, total_no_elements, NELEE, total_no_links
      INTEGER, INTENT(IN) :: MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS
      INTEGER, INTENT(IN) :: NCON, NCONEE

      INTEGER, INTENT(IN) :: NUM_CATEGORIES_TYPES(NCONEE)
      INTEGER, INTENT(IN) :: NTAB(MAX_NUM_CATEGORY_TYPES, NCONEE)
      INTEGER, INTENT(INOUT) :: NCATTY(NELEE, NCONEE)
      LOGICAL, INTENT(IN) :: ISCNSV(NCONEE)

      DOUBLE PRECISION, INTENT(INOUT) :: TABLE_CONCENTRATION(MAX_NUM_CATEGORY_TYPES, &
                                                             MAX_NUM_DATA_PAIRS, NCONEE)
      DOUBLE PRECISION, INTENT(INOUT) :: TABLE_WATER_DEPTH(MAX_NUM_CATEGORY_TYPES, &
                                                           MAX_NUM_DATA_PAIRS, NCONEE)

      LOGICAL, INTENT(INOUT) :: LDUM(1)  !! Workspace/Flag

      ! --- Local Variables ---
      INTEGER :: ICOL1, NERR, NELMTY, NTBL, I, J
      INTEGER, PARAMETER :: IUNDEF = 0   !! Unused subscript marker for ALCHK diagnostics
      DOUBLE PRECISION :: PREVDP    !! Previous depth for monotonicity check

      ! Constant arrays required by ALCHKI/ALCHK interfaces
      INTEGER :: IZERO(1)

      ! 0. Preliminaries
      ! --- Data Initialisation ---
      IZERO = (/0/)
      NERR = 0
      ICOL1 = total_no_links + 1

      ! 1. Check the data used to calculate the spatially variable
      ! contaminant concentrations

      contam_loop: DO I = 1, NCON

         IF (ISCNSV(I)) THEN

            ! *NCATTY
            ncatty_loop: DO J = ICOL1, total_no_elements
               CALL ALCHKI(ERRLVL_error, 2103, CPR, J, J, IUNDEF, IUNDEF, &
                           'NCATTY(iel)', 'GT', IZERO, NCATTY(J:J, I), NERR, LDUM(1:1))
            END DO ncatty_loop

            ! *TABLE_WATER_DEPTH
            ! The table of depths must have a first depth equal to zero,
            ! thereafter the depth must increase
            category_loop1: DO NELMTY = 1, NUM_CATEGORIES_TYPES(I)

               CALL ALCHK(ERRLVL_error, 2104, CPR, NELMTY, NELMTY, 1, IUNDEF, &
                          'TABLE_WATER_DEPTH[NUM_CATEGORIES_TYPES,1]', 'EQ', ZERO1, ZERO, &
                          TABLE_WATER_DEPTH(NELMTY:NELMTY, 1, I), NERR, LDUM(1:1))

               table_depth_loop: DO NTBL = 2, NTAB(NELMTY, I)
                  PREVDP = TABLE_WATER_DEPTH(NELMTY, NTBL - 1, I)
                  CALL ALCHK(ERRLVL_error, 2105, CPR, NELMTY, NELMTY, NTBL, IUNDEF, &
                             'TABLE_WATER_DEPTH[NUM_CATEGORIES_TYPES,ntab]', 'GT', (/PREVDP/), &
                             ZERO, TABLE_WATER_DEPTH(NELMTY:NELMTY, NTBL, I), NERR, LDUM(1:1))
               END DO table_depth_loop

            END DO category_loop1

            ! *TABLE_CONCENTRATION
            ! Each value in the table of concentrations must be >= 0
            category_loop2: DO NELMTY = 1, NUM_CATEGORIES_TYPES(I)
               table_conc_loop: DO NTBL = 1, NTAB(NELMTY, I)
                  CALL ALCHK(ERRLVL_error, 2106, CPR, NELMTY, NELMTY, NTBL, IUNDEF, &
                             'TABLE_CONCENTRATION[nmne,ntab]', 'GE', ZERO1, ZERO, &
                             TABLE_CONCENTRATION(NELMTY:NELMTY, NTBL, I), NERR, LDUM(1:1))
               END DO table_conc_loop
            END DO category_loop2

         END IF

      END DO contam_loop

      ! 2. Epilogue
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 2107, CPR, 0, 0, 'Error(s) detected while checking static/initial interface')
      END IF

   END SUBROUTINE MUERR2

END MODULE cm_input

