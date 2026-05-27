!> summary: Contaminant transport in columns, links, sediment, and plants.
!>
!> This module implements SHETRAN's legacy contaminant component. It reads
!> contaminant input, updates contaminant concentrations over one timestep,
!> prepares water-flow and storage terms for land columns and channel links,
!> solves column and link transport equations, applies linear or nonlinear
!> sorption/retardation, exchanges contaminant with sediment compartments, and
!> updates plant uptake and plant-compartment concentrations.
!>
!> The transport formulation is an advection-dispersion-reaction style mass
!> balance with storage, decay/generation, plant uptake, sediment exchange, and
!> source/sink terms. The nonlinear sorption branches use the Freundlich-type
!> power-law exponent `GNN` read from the CM input file. The CM input format is
!> described in the manual's Contaminant Migration Components section.
!>
!> Soil-column transport is split into a mobile/dynamic region and a dead-space
!> region. [[colm]] assembles the column transport problem, [[colmw]] prepares
!> water-flow and storage terms, [[colmsm]] prepares source/sink and sorption
!> terms, and [[slvclm]] solves the resulting tridiagonal system. Channel-link
!> transport is handled by [[linkw]], [[linksm]], and [[link]], with sediment
!> exchange active when the sediment component is present. Plant uptake is
!> prepared through [[plprep]], [[plcolm]], and [[plant]] when the plant option is
!> active.
!>
!> `CMSIM` calls [[mnmod:mncont]] before contaminant transport when the mineral
!> nitrogen option is enabled. In that case nitrate process source/sink terms
!> are supplied through the contaminant equation rather than being solved as an
!> independent transport component.
!>
!> @warning The manual includes soil/contaminant input fields for mobile-water
!> fraction and dispersion (`CM57`, `CM59`, and `CM61`). The current routines
!> [[phi]] and [[disp]] do not yet use those tables: `PHI` returns `0.5` and
!> `DISP` returns `3.0D-8`.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1998 | GP/RAH/SB | 3.4-4.2 | Developed and reorganised CM transport routines. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the CM `COLM` and `LINK` `.F` files into this Fortran 90 module. |
!> | 2020-03-05 | SvenB | - | Removed the complete `SGLOBAL` include in favour of selected imports. |
!> @endhistory
MODULE CMmod
   USE SGLOBAL, ONLY :                                                             &
      nlf=>total_no_links, area=>cellarea, NEL=>total_no_elements,                  &
      NOTZERO, ZERO, ONE, TWO, HALF,                                                &
      ISZERO, GTZERO, LTZERO, GEZERO, DYQQ, DXQQ, ZGRUND, ERROR
   USE OCMOD2,  ONLY : hrf=>hrfzz
! USE AL_P
   USE AL_C
   USE AL_G
   USE IS_CC
   USE UTILSMOD, ONLY : TRIDAG
   USE IS_CC
   USE mod_load_filedata, ONLY: ALALLI, ALINIT, ALREDC, ALREDF, ALREDI, ALREDL, ALRED2
! USE mod_load_filedata, ONLY:ERROR, ERRC, ERRNEE, ERRTOT !AD NEEDS THIS  , HELPPATH
   USE UTILSMOD, ONLY : DCOPY
   USE MNMOD, only : MNCONT
   IMPLICIT NONE

   INTEGER :: JBK, JFLINK, JSOL(LLEE), NWORK(4), NLINKA, NCWELL
   DOUBLEPRECISION :: VELDUM (LLEE), QQQWEL, QQQWL1, QQRV(LLEE), ROH(LLEE)
   LOGICAL :: ISBDY (4)
!COMMON / WTOCI / JBK, JFLINK, JSOL, NWORK, NLINKA, NCWELL
!COMMON / WTOC / VELDUM, QQQWEL, QQQWL1, QQRV, ROH
!COMMON / WTOCL / ISBDY
   INTEGER:: count = 0
   INTEGER :: LWORK(6), NBK(2), nwell
   LOGICAL :: islk(2)
   DOUBLEPRECISION ::  qqqdum, QQQSL1
!COMMON / LK1 / ISLK (2), LWORK (6), NBK (2), qqqdum, QQQSL1
!common / temp / nwell
! nwell and qqqdum used in temporary irrigation code.


   PRIVATE
   PUBLIC :: CMSIM, CMFIN, CMRD
CONTAINS




!> Finalises the contaminant component at the end of a simulation.
!>
!> This legacy hook is called from the water-flow component, but the current
!> implementation has no file handles, allocations, or accumulated state to
!> release, so it returns immediately.
   SUBROUTINE CMFIN
!                             CALLED FROM WATER FLOW COMPONENTS.
!                             TIDIES UP AT END OF SIMULATION.
      RETURN
   END SUBROUTINE CMFIN



!> Reads contaminant input data and initialises contaminant-control arrays.
!>
!> `CMRD` reads the CM data file sections for contaminant count, boundary
!> categories, initial concentrations, soil/sediment sorption parameters,
!> decay/generation rates, exchange coefficients, lookup tables, and flags for
!> conservative and nonlinear adsorption behaviour.
!>
!> The read sequence follows manual records `CM1`-`CM61`:
!>
!> | Records | Main state populated |
!> |:--------|:---------------------|
!> | `CM1`-`CM5` | Title, number of contaminants `NCON`, and base-boundary mode `ISFLXB`. |
!> | `CM7`-`CM11` | Bottom contaminant cell per land column, stored in `NCOLMB`; `NCED=-1` uses the column base from `NLYRBE`. |
!> | `CM13`-`CM23` | Adsorption flag, bed-layer depths, and contaminant/soil/sediment data-set counts. |
!> | `CM25`-`CM26e` | Initial concentrations, including optional element maps and depth tables. |
!> | `CM27`-`CM39` | Rainfall, external-flow, base, flux-boundary, and dry-deposition concentrations. |
!> | `CM41`-`CM61` | Sorption, decay, exchange, sediment-fraction, and mobile-water/dispersion data. |
!>
!> `ISFLXB` controls where base-concentration records are stored:
!>
!> | `ISFLXB` | `CM33`/`CM37` destination | Meaning |
!> |:---------|:-------------------------|:--------|
!> | `.TRUE.` | `CCAPR` | Concentration convected with base flux. |
!> | `.FALSE.` | `CCAPB` | Prescribed concentration at the base cell. |
!>
!> @note `CM57`, `CM59`, and `CM61` are read into local mobile-water and
!> dispersion tables, but the current transport helper functions [[phi]] and
!> [[disp]] still return fixed constants.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1995-03-22 | RAH | 3.4.2 | Created from the 1995-02-01 CM reader. |
!> | 1997-05-01 | SB | 4.2 | Updated CM input handling as noted in the legacy source. |
!> @endhistory
   SUBROUTINE CMRD (CMD, CPR, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, NEL, NLF, NLFEE, &
      NSEE, NS, NSEDEE, NSED, MAX_NUM_DATA_PAIRS, NX, NXEE, NYEE, NY, NLYRBE, ICMXY, &
      ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES,  NCATTY, NCON, NCOLMB, NTAB, &
      DBS, DBDI, CCAPI, CCAPE, CCAPR, CCAPB,TABLE_CONCENTRATION, TABLE_WATER_DEPTH, IIICF, SOFN, &
      GNN, GGLMSO, ALPHBD, ALPHBS, KDDLS, ALPHA, FADS, &
      ISCNSV, IDUM, DUMMY)
!
!----------------------------------------------------------------------*
!
!  Read CM data input file
!
!----------------------------------------------------------------------*
!
!
      USE CONT_CC, ONLY:CCAPIN
!
      INTEGER, INTENT(IN) :: CMD                    !! CM input file unit.
      INTEGER, INTENT(IN) :: CPR                    !! CM print/output file unit.
      INTEGER, INTENT(IN) :: MAX_NUM_CATEGORY_TYPES !! Maximum number of category types in lookup tables.
      INTEGER, INTENT(IN) :: NCONEE                 !! Allocated contaminant-category dimension.
      INTEGER, INTENT(IN) :: NELEE                  !! Allocated element dimension.
      INTEGER, INTENT(IN) :: NEL                    !! Number of elements.
      INTEGER, INTENT(IN) :: NLF                    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE                  !! Allocated link dimension.
      INTEGER, INTENT(IN) :: NSEE                   !! Allocated soil/sediment data-set dimension.
      INTEGER, INTENT(IN) :: NS                     !! Number of soil data sets.
      INTEGER, INTENT(IN) :: NSEDEE                 !! Allocated sediment-size dimension.
      INTEGER, INTENT(IN) :: NSED                   !! Number of sediment size fractions.
      INTEGER, INTENT(IN) :: MAX_NUM_DATA_PAIRS     !! Maximum number of depth-concentration table pairs.
      INTEGER, INTENT(IN) :: NX                     !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE                   !! Allocated grid-column dimension.
      INTEGER, INTENT(IN) :: NYEE                   !! Allocated grid-row dimension.
      INTEGER, INTENT(IN) :: NY                     !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)        !! Grid-to-element map.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)        !! Link-bank element map.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Element reference map used by array readers.
      INTEGER, INTENT(IN) :: NLYRBE(NLF + 1:NEL)    !! Bottom VSS layer for each land element.
      LOGICAL, INTENT(IN) :: BEXBK                  !! Explicit-bank flag.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)          !! Link-orientation flags.
      INTEGER, INTENT(OUT) :: NCON                  !! Number of contaminants read from `CM3`.
      INTEGER, INTENT(OUT) :: NUM_CATEGORIES_TYPES(NCONEE) !! Number of category types per contaminant.
      INTEGER, INTENT(OUT) :: NCATTY(NELEE, NCONEE) !! Category index assigned to each element and contaminant.
      INTEGER, INTENT(OUT) :: NCOLMB(NLF + 1:NEL)   !! Bottom contaminant cell for each land column.
      INTEGER, INTENT(OUT) :: NTAB(MAX_NUM_CATEGORY_TYPES, NCONEE) !! Table-pair counts by category and contaminant.
      DOUBLEPRECISION, INTENT(OUT) :: DBS           !! Bed-surface layer depth.
      DOUBLEPRECISION, INTENT(OUT) :: DBDI          !! Initial deep-bed layer depth.
      DOUBLEPRECISION, INTENT(OUT) :: CCAPI(NCONEE) !! Rainfall concentration by contaminant.
      DOUBLEPRECISION, INTENT(OUT) :: CCAPE(NELEE, NCONEE) !! External-flow concentration by element and contaminant.
      DOUBLEPRECISION, INTENT(OUT) :: CCAPR(NELEE, NCONEE) !! Base flux concentration by element and contaminant.
      DOUBLEPRECISION, INTENT(OUT) :: CCAPB(NELEE, NCONEE) !! Prescribed base-cell concentration by element/contaminant.
      DOUBLEPRECISION, INTENT(OUT) :: TABLE_CONCENTRATION(MAX_NUM_CATEGORY_TYPES, &
         MAX_NUM_DATA_PAIRS, NCONEE) !! Initial-concentration table values.
      DOUBLEPRECISION, INTENT(OUT) :: TABLE_WATER_DEPTH(MAX_NUM_CATEGORY_TYPES, &
         MAX_NUM_DATA_PAIRS, NCONEE) !! Initial-concentration table depths.
      DOUBLEPRECISION, INTENT(OUT) :: IIICF(NCONEE) !! Dry-deposition rate by contaminant.
      DOUBLEPRECISION, INTENT(OUT) :: SOFN(NSEE, 3) !! Soil sediment-fraction data.
      DOUBLEPRECISION, INTENT(OUT) :: GNN(NCONEE) !! Freundlich power by contaminant.
      DOUBLEPRECISION, INTENT(OUT) :: GGLMSO(NCONEE) !! First-order decay/generation coefficient by contaminant.
      DOUBLEPRECISION, INTENT(OUT) :: ALPHBD(NCONEE) !! Exchange coefficient between channel bed layers.
      DOUBLEPRECISION, INTENT(OUT) :: ALPHBS(NCONEE) !! Exchange coefficient between stream water and bed surface.
      DOUBLEPRECISION, INTENT(OUT) :: KDDLS(NSEDEE, NCONEE) !! Link/sediment distribution coefficients.
      DOUBLEPRECISION, INTENT(OUT) :: ALPHA(NSEE, NCONEE) !! Soil dynamic/dead-space exchange coefficient.
      DOUBLEPRECISION, INTENT(OUT) :: FADS(NSEE, NCONEE) !! Dynamic adsorption-site fraction.
      DOUBLEPRECISION PHIDAT (NSEE), DIFDAT (NCONEE), DISPDT (NSEE, &
         NCONEE)
! LOGICAL :: ISFLXB, ISADNL
      LOGICAL, INTENT(OUT) :: ISCNSV(NCONEE) !! Spatial initial-concentration flag by contaminant.
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT) :: IDUM !! Integer workspace used by AL input readers.
      DOUBLEPRECISION, INTENT(INOUT) :: DUMMY(NELEE) !! Floating-point workspace used by AL input readers.
!
! Locals, etc
      INTEGER :: FATAL, rubbish (1, 1), j
      PARAMETER (FATAL = 1)
!
      INTEGER :: I, IEL, INDX, NC, NCBC, NCED, NCLBND, NCONCM, NCONT
      INTEGER :: NDATA, NFEX, NMAX (3), NREQ, NSCM, NSEDCM, NTB, NTBL, &
         SOIL
      LOGICAL :: LDUM (1)
      CHARACTER (80) :: CDUM(1)
      CHARACTER(132) :: MSG
!
!----------------------------------------------------------------------*
!
!
! Preliminaries
! -------------
!
!     * Check status of data file
      CALL ALRED2 (0, CMD, CPR, 'CMD')
!
!     * Print title for contaminant simulation
      CALL ALREDC (0, CMD, CPR, ':CM1', 1, 1, CDUM)
      WRITE (CPR, '(/1X,A/)') CDUM
!
!
! Some Static Data
! ----------------
!
!     * Number of contaminants
      CALL ALREDI (0, CMD, CPR, ':CM3', 1, 1, IDUM)
      NCON = IDUM (1)
!
!     * Flux boundary condition at base of column?
      CALL ALREDL (0, CMD, CPR, ':CM5', 1, 1, LDUM)
      ISFLXB = LDUM (1)
!
!
! Bottom Cell Data
! ----------------
!
!     * Default cell number at base of columns (-1 special: see below)
      CALL ALREDI (0, CMD, CPR, ':CM7', 1, 1, IDUM)
      NCED = IDUM (1)
!
!     * Number of columns where bottom cell number is not default value
      CALL ALREDI (0, CMD, CPR, ':CM9', 1, 1, IDUM)
      NCLBND = IDUM (1)
!
      IF (NCLBND.GT.0) THEN
!        * Column numbers & bottom cell numbers for those columns
         NREQ = 2 * NCLBND
         IF (NREQ.GT.NELEE) GOTO 8090
         CALL ALREDI (0, CMD, CPR, ':CM11', 2, NCLBND, IDUM)
      ENDIF
!
!     * Assemble the above information: set the default ...
      DO 110 IEL = NLF + 1, NEL
         IF (NCED.EQ. - 1) THEN
!           * special case
            NCOLMB (IEL) = NLYRBE (IEL)
         ELSE
            NCOLMB (IEL) = NCED
         ENDIF
110   END DO
!       ... then overwrite any non-default columns
      INDX = 1
      DO 114 I = 1, NCLBND
         IEL = IDUM (INDX)
         IF (IEL.LE.NLF.OR.IEL.GT.NEL) GOTO 8110
         NCOLMB (IEL) = IDUM (INDX + 1)
         INDX = INDX + 2
114   END DO
!
!
! More Static & Initialization Data
! ---------------------------------
!
!     * Non-linear adsorption?
      CALL ALREDL (0, CMD, CPR, ':CM13', 1, 1, LDUM)
      ISADNL = LDUM (1)
!
!     * Depth of bed surface layer
      CALL ALREDF (0, CMD, CPR, ':CM15', 1, 1, DUMMY)
      DBS = DUMMY (1)
!
!     * Initial depth of bed deep layer
      CALL ALREDF (0, CMD, CPR, ':CM17', 1, 1, DUMMY)
      DBDI = DUMMY (1)
!
!
! Local Data
! ----------
!
!     * Number of contaminants for which there are property data
      CALL ALREDI (0, CMD, CPR, ':CM19', 1, 1, IDUM)
      NCONCM = IDUM (1)
!
!     * Number of soil types for which there are contaminant data
      CALL ALREDI (0, CMD, CPR, ':CM21', 1, 1, IDUM)
      NSCM = IDUM (1)
!
!     * Number of sediment sizes for which there are contaminant data
      CALL ALREDI (0, CMD, CPR, ':CM23', 1, 1, IDUM)
      NSEDCM = IDUM (1)
!
!     * Set maximum admissible values for the above
      NMAX (1) = MIN (NCONEE, NELEE)
!      NMAX(2) = MIN( NSEE, NELEE/4, (NELEE/NCONCM))
      nmax (2) = nsee
!      NMAX(3) = MIN( NSEDEE, (NELEE/NCONCM) - 1 )
      nmax (3) = nsedee
!
! Initial Conditions
! ------------------
!






      IF (NCONCM.LT.1.OR.NCONCM.GT.NMAX (1) ) GOTO 8190
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! New code by SB for spatially distributed initial conditions
! -----------------------------------------------------------
! New variables
! External  ALALLI
! Input Variables
!     INTEGER MAX_NUM_CATEGORY_TYPES,NLFEE,MAX_NUM_DATA_PAIRS,NX,NXEE,NY,
!     INTEGER ICMXY(NXEE,NY),ICMBK(NLFEE,2),ICMREF(NELEE,4,2:2)
!     LOGICAL BEXBK,LINKNS(NLFEE)
! Output Variables
!      INTEGER NUM_CATEGORIES_TYPES(NCONEE),NTAB(MAX_NUM_CATEGORY_TYPES,NCONEE)
!      INTEGER NCATTY(NELEE,NCONEE)
!      DOUBLEPRECISIONTABLE_CONCENTRATION(MAX_NUM_CATEGORY_TYPES,MAX_NUM_DATA_PAIRS,NCONEE)
!      DOUBLEPRECISION TABLE_WATER_DEPTH(MAX_NUM_CATEGORY_TYPES,MAX_NUM_DATA_PAIRS,NCONEE)
!      LOGICAL ISCNSV(NCONEE)
! Local Variables
!     INTEGER NC,NDATA,NTBL,NTB
! Workspace
!     DUMMY wil work corrctly if NELEE >= 2 * MAX_NUM_DATA_PAIRS
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!
      DO 260 I = 1, NCONCM
!
!        Is the initial contaminant concentration spatially
!        variable ?
         CALL ALREDL (0, CMD, CPR, ':CM25', 1, 1, LDUM)
         ISCNSV (I) = LDUM (1)
!
         IF (.NOT.ISCNSV (I) ) THEN
!
!           * Initial concentration throughout catchment
            CALL ALREDF (0, CMD, CPR, ':CM26', 1, 1, CCAPIN (I) )
!
         ELSE
!
!           * Initial concentration for link elements
            CALL ALREDF (0, CMD, CPR, ':CM26a', 1, 1, CCAPIN (I) )
            DO J = 1, NLF
               !"" NCATTY (J, I) = CCAPIN (I)  !AD
            ENDDO
!
!          * Find out how many typical element catagories
            CALL ALREDI (0, CMD, CPR, ':CM26b', 1, 1, IDUM)
            NUM_CATEGORIES_TYPES (I) = IDUM (1)
            IF ( (NUM_CATEGORIES_TYPES (I) .GT.MAX_NUM_CATEGORY_TYPES) .OR. (NUM_CATEGORIES_TYPES (I) .LE.0) ) THEN
               CALL ERROR (FATAL, 2101, CPR, 0, 0, 'Error in NUM_CATEGORIES_TYPES in :CM26 in CM data file')
            ENDIF
!
!           * Read the catagory type for each element into the element
!           * number
            CALL ALALLI (NUM_CATEGORIES_TYPES (I) , CMD, CPR, ':CM26c', NEL, NLF, NX, &
               NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
               NCATTY (NLF + 1, I) , IDUM)
!
!           * Table of values for each typical element
            DO 930 NC = 1, NUM_CATEGORIES_TYPES (I)
!               CALL ALREDI(0,CMD,CPR,':CM26d',1,1,NTBL)
               CALL ALREDI (0, CMD, CPR, ':CM26d', 1, 1, rubbish)
               ntbl = rubbish (1, 1)
!
               NTAB (NC, I) = NTBL
               IF ( (NTBL.GT.MAX_NUM_DATA_PAIRS) .OR. (NTBL.LE.0) ) THEN
                  CALL ERROR (FATAL, 2102, CPR, 0, 0, 'Error in NTBL in :CM26a in CM data file')
               ENDIF
!
               NDATA = NTBL * 2
               CALL ALREDF (0, CMD, CPR, ':CM26e', NDATA, 1, DUMMY)
               DO 940 NTB = 1, NTBL
                  TABLE_WATER_DEPTH (NC, NTB, I) = DUMMY (2 * NTB - 1)
                  TABLE_CONCENTRATION (NC, NTB, I) = DUMMY (2 * NTB)
940            END DO
930         END DO
         ENDIF
!


260   END DO
!
!^^^^^^^^^^^^^^^^^^^^^^
! End of new code by SB
! ---------------------
!^^^^^^^^^^^^^^^^^^^^^^
!
! Boundary Conditions
! -------------------
!
!     * Concentrations in rainfall
      CALL ALREDF (0, CMD, CPR, ':CM27', NCONCM, 1, CCAPI)
!
!     * Number of columns which receive flow from outside catchment
      CALL ALREDI (0, CMD, CPR, ':CM29', 1, 1, IDUM)
      NFEX = IDUM (1)
!
      IF (NFEX.GT.0) THEN
!        * Numbers of those columns, and concentrations in the flows
!        * (read list index as extra column of floating-point data)
         NREQ = (1 + NCONCM) * NFEX
         IF (NREQ.GT.NELEE) GOTO 8290
         CALL ALREDF (0, CMD, CPR, ':CM31', 1 + NCONCM, NFEX, DUMMY)
      ENDIF
!
!     * Assemble the above info
      DO 310 NCONT = 1, NCONCM
         CALL ALINIT (ZERO, NEL - NLF, CCAPE (NLF + 1, NCONT) )
310   END DO
      INDX = 1
      DO 312 I = 1, NFEX
         IEL = NINT (DUMMY (INDX) )
         IF (IEL.LE.NLF.OR.IEL.GT.NEL) GOTO 8310
         CALL DCOPY (NCONCM, DUMMY (INDX + 1), 1, CCAPE (IEL, 1), &
            NELEE)
         INDX = INDX + 1 + NCONCM
312   END DO
!
!     * Default concentration at or convected into bases of columns
      CALL ALREDF (0, CMD, CPR, ':CM33', NCONCM, 1, DUMMY)
      DO 330 NCONT = 1, NCONCM
         IF (ISFLXB) THEN
            CALL ALINIT (DUMMY (NCONT), NEL - NLF, CCAPR (NLF + 1, &
               NCONT) )
         ELSE
            CALL ALINIT (DUMMY (NCONT), NEL - NLF, CCAPB (NLF + 1, &
               NCONT) )
         ENDIF
330   END DO
!
!     * Number of columns where base concentration is not default value
      CALL ALREDI (0, CMD, CPR, ':CM35', 1, 1, IDUM)
      NCBC = IDUM (1)
!
      IF (NCBC.GT.0) THEN
!        * Numbers and concentrations for those columns
!        * (read list index as extra column of floating-point data)
         NREQ = (1 + NCONCM) * NCBC
         IF (NREQ.GT.NELEE) GOTO 8350
         CALL ALREDF (0, CMD, CPR, ':CM37', 1 + NCONCM, NCBC, DUMMY)
         INDX = 1
         DO 370 I = 1, NCBC
            IEL = NINT (DUMMY (INDX) )
            IF (IEL.LE.NLF.OR.IEL.GT.NEL) GOTO 8370
            IF (ISFLXB) THEN
               CALL DCOPY (NCONCM, DUMMY (INDX + 1), 1, CCAPR (IEL, 1), &
                  NELEE)
            ELSE
               CALL DCOPY (NCONCM, DUMMY (INDX + 1), 1, CCAPB (IEL, 1), &
                  NELEE)
            ENDIF
            INDX = INDX + 1 + NCONCM
370      END DO
      ENDIF
!
!     * Rate of dry deposition, for each contaminant
      CALL ALREDF (0, CMD, CPR, ':CM39', NCONCM, 1, IIICF)
!
!
! Some Soil Properties
! --------------------
!
      IF (NSCM.LT.1.OR.NSCM.GT.NMAX (2) ) GOTO 8210
!
!     * 3 size fractions (used only if SY module inactive)
!     * (read soil index as extra column of floating-point data)
      CALL ALREDF (0, CMD, CPR, ':CM41', 4, NSCM, DUMMY)
      INDX = 1
      DO 410 I = 1, NSCM
         SOIL = NINT (DUMMY (INDX) )
         IF (SOIL.LT.1.OR.SOIL.GT.NSCM) GOTO 8410
         CALL DCOPY (3, DUMMY (INDX + 1), 1, SOFN (SOIL, 1), NSEE)
         INDX = INDX + 4
410   END DO
!
!
! Some Contaminant Properties
! ---------------------------
!
!     * Freundlich isotherm power constant
      CALL ALREDF (0, CMD, CPR, ':CM43', NCONCM, 1, GNN)
!
!     * Chemical decay constant
      CALL ALREDF (0, CMD, CPR, ':CM45', NCONCM, 1, GGLMSO)
!
!     * Coefficients for exchange between bed layers
      CALL ALREDF (0, CMD, CPR, ':CM47', NCONCM, 1, ALPHBD)
!
!     * Coefficients for exchange between water and bed
      CALL ALREDF (0, CMD, CPR, ':CM49', NCONCM, 1, ALPHBS)
!
!
! More Contaminant/Sediment/Soil Properties
! -----------------------------------------
!
      IF (NSEDCM.LT.1.OR.NSEDCM.GT.NMAX (3) ) GOTO 8230
!
!     * Reference Kd for each particle size
!     * (read contaminant index as extra column of floating-point data)
      CALL ALREDF (0, CMD, CPR, ':CM51', 1 + NSEDCM, NCONCM, DUMMY)
      INDX = 1
      DO 510 I = 1, NCONCM
         NCONT = NINT (DUMMY (INDX) )
         IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) GOTO 8510
         CALL DCOPY (NSEDCM, DUMMY (INDX + 1), 1, KDDLS (1, NCONT), &
            1)
         INDX = INDX + 1 + NSEDCM
510   END DO
!
!     * Coefficients for exchange between soil regions
      CALL ALREDF (0, CMD, CPR, ':CM53', 1 + NSCM, NCONCM, DUMMY)
      INDX = 1
      DO 530 I = 1, NCONCM
         NCONT = NINT (DUMMY (INDX) )
         IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) GOTO 8530
         CALL DCOPY (NSCM, DUMMY (INDX + 1), 1, ALPHA (1, NCONT), &
            1)
         INDX = INDX + 1 + NSCM
530   END DO
!
!     * Fraction of adsorption sites in dynamic region
      CALL ALREDF (0, CMD, CPR, ':CM55', 1 + NSCM, NCONCM, DUMMY)
      INDX = 1
      DO 550 I = 1, NCONCM
         NCONT = NINT (DUMMY (INDX) )
         IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) GOTO 8550
         CALL DCOPY (NSCM, DUMMY (INDX + 1), 1, FADS (1, NCONT), &
            1)
         INDX = INDX + 1 + NSCM
550   END DO
!
!     * Fraction of pore water in dynamic region
      CALL ALREDF (0, CMD, CPR, ':CM57', NSCM, 1, PHIDAT)
!
!     * Diffusion coefficient
      CALL ALREDF (0, CMD, CPR, ':CM59', NCONCM, 1, DIFDAT)
!
!     * Dispersivity
      CALL ALREDF (0, CMD, CPR, ':CM61', 1 + NSCM, NCONCM, DUMMY)
      INDX = 1
      DO 610 I = 1, NCONCM
         NCONT = NINT (DUMMY (INDX) )
         IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) GOTO 8610
         CALL DCOPY (NSCM, DUMMY (INDX + 1), 1, DISPDT (1, NCONT), &
            1)
         INDX = INDX + 1 + NSCM
610   END DO
!
!
! Epilogue
! -----------
!
!     * Close the data file
      CALL ALRED2 (1, CMD, CPR, 'CMD')
!
!     * Is everything defined?
      IF (NCONCM.LT.NCON.OR.NSCM.LT.NS.OR.NSEDCM.LT.NSED) GOTO 8000
!
      RETURN
!
!
! Error Branches
! --------------
!
!     * Not enough data
8000  WRITE (MSG, 9800) NCONCM, NSCM, NSEDCM, NCON, NS, NSED
      CALL ERROR (FATAL, 3008, CPR, 0, 0, MSG)
!
!     * Insufficient workspace
8090  WRITE (MSG, 9809) NELEE, NREQ, 'non-default columns', 'CM9: NCLBND ', NCLBND
      CALL ERROR (FATAL, 3001, CPR, 0, 0, MSG)
!
!     * Invalid column number
8110  WRITE (MSG, 9811) IEL, 'CM11', 'column element'
      CALL ERROR (FATAL, 3002, CPR, 0, 0, MSG)
!
!     * Too many contaminants
8190  WRITE (MSG, 9819) 'contaminants', 'CM19: NCONCM', NCONCM, NMAX (1)
      CALL ERROR (FATAL, 3003, CPR, 0, 0, MSG)
!
!     * Too many soil types
8210  WRITE (MSG, 9819) 'soil types', 'CM21: NSCM', NSCM, NMAX (2)
      CALL ERROR (FATAL, 3004, CPR, 0, 0, MSG)
!
!     * Too many sediment sizes
8230  WRITE (MSG, 9819) 'sediment sizes', 'CM23: NSEDCM', NSEDCM, NMAX ( &
         3)
      CALL ERROR (FATAL, 3005, CPR, 0, 0, MSG)
!
!     * Insufficient workspace
8290  WRITE (MSG, 9809) NELEE, NREQ, 'flow-receiving columns', 'CM29: NFEX', NFEX
      CALL ERROR (FATAL, 3001, CPR, 0, 0, MSG)
!
!     * Invalid column number
8310  WRITE (MSG, 9811) IEL, 'CM31', 'column element'
      CALL ERROR (FATAL, 3002, CPR, 0, 0, MSG)
!
!     * Insufficient workspace
8350  WRITE (MSG, 9809) NELEE, NREQ, 'non-default columns', 'CM35: NCBC' &
      &, NCBC
      CALL ERROR (FATAL, 3001, CPR, 0, 0, MSG)
!
!     * Invalid column number
8370  WRITE (MSG, 9811) IEL, 'CM37', 'column element'
      CALL ERROR (FATAL, 3002, CPR, 0, 0, MSG)
!
!     * Invalid soil type
8410  WRITE (MSG, 9811) SOIL, 'CM41', 'soil type'
      CALL ERROR (FATAL, 3006, CPR, 0, 0, MSG)
!
!     * Invalid contaminant number
8510  WRITE (MSG, 9811) NCONT, 'CM51', 'contaminant number'
      CALL ERROR (FATAL, 3007, CPR, 0, 0, MSG)
!
!     * Invalid contaminant number
8530  WRITE (MSG, 9811) NCONT, 'CM53', 'contaminant number'
      CALL ERROR (FATAL, 3007, CPR, 0, 0, MSG)
!
!     * Invalid contaminant number
8550  WRITE (MSG, 9811) NCONT, 'CM55', 'contaminant number'
      CALL ERROR (FATAL, 3007, CPR, 0, 0, MSG)
!
!     * Invalid contaminant number
8610  WRITE (MSG, 9811) NCONT, 'CM61', 'contaminant number'
      CALL ERROR (FATAL, 3007, CPR, 0, 0, MSG)
!
!
! Formats
! -------
!
9800  FORMAT ('No. of contaminants/soils/sediments with data' &
      &       ,' (CM19-23: NCONCM/NSCM/NSEDCM = ',2(I3,'/'),I3,')' &
      &       ,' must be at least ',2(I3,'/'),I3)
!
!     * length 91+2A
9809  FORMAT ('Insufficient workspace (have NELEE =',I6,', need',I6,')' &
      &       ,' for the number of ',A,' given (',A,' =',I6,')')
!
!     * length 57+2A
9811  FORMAT ('Index',I6,' (given as part of data item ',A,')' &
      &       ,' is not a valid ',A)
!
!     * length 73+2A
9819  FORMAT ('Number of ',A,' with data (',A,' =',I6,')' &
      &       ,' must be positive & not greater than',I6)
!
!
   END SUBROUTINE CMRD



!> Runs the contaminant component for the whole catchment for one timestep.
!>
!> This is the entry point to the contaminant components when updating catchment
!> contaminant concentrations. The controller derives link flow directions when
!> sediment is inactive, calls the mineral-nitrogen model when enabled, prepares
!> plant uptake terms, loops over land columns and links, and stores updated
!> concentrations for use in the next timestep.
!>
!> `ISSDON` indicates whether the sediment component has already supplied link
!> sediment-flow information for the current timestep. If sediment is inactive,
!> `CMSIM` derives the link inflow/outflow directions directly from `QOC`.
!>
!> | Link orientation | `QLINK(link,1)` | `QLINK(link,2)` |
!> |:-----------------|:----------------|:----------------|
!> | `LINKNS=.TRUE.` | `-QOC(link,2)` | `QOC(link,4)` |
!> | `LINKNS=.FALSE.` | `-QOC(link,1)` | `QOC(link,3)` |
!>
!> The contaminant timestep used by the finite-difference routines is
!>
!> \[
!> TSE = D0\,DTUZ/Z2SQ .
!> \]
!>
!> Processing then runs in sorted element order: land elements call [[colmw]]
!> and [[colmsm]], while channel links call [[linkw]] and [[linksm]]. At the
!> end of the step, current concentrations are copied into `CCCCO`/`SSSSO` as
!> previous-time-level state for the next call; `RSZWLO` similarly stores the
!> current well/spring flux state for land elements.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought `IMPLICIT` declarations from `AL.P`. |
!> | 1995-03-22 | RAH | 4.0 | Replaced `RSZWEL` with `QVSWEL` for the new VSS. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing. |
!> @endhistory
   SUBROUTINE CMSIM (ISSDON)
! Commons and constants
      USE SED_CS
      USE CONT_CC
      USE COLM_C1
      USE COLM_CO
      USE COLM_CG
      USE LINK_CW
      USE PLANT_CC
      USE SGLOBAL, ONLY       : uznow
      USE AL_D, ONLY       : TA
!                             INCLUDE THE PARAMETER STATEMENTS
!                             AND THE WATER/CONTAMINANT INTERFACE
!                             COMMON BLOCKS
!
      LOGICAL, INTENT(IN) :: ISSDON !! True when the sediment component has already supplied link sediment-flow information.
      INTEGER :: NLINK, NDUM, NELM, NCONT, NCE
!
!----------------------------------------------------------------------*
!
      IF (.NOT.ISSDON) THEN
         DO 100 NLINK = 1, NLF
            IF (LINKNS (NLINK) ) THEN
               QLINK (NLINK, 1) = - QOC (NLINK, 2)
               QLINK (NLINK, 2) = QOC (NLINK, 4)
            ELSE
               QLINK (NLINK, 1) = - QOC (NLINK, 1)
               QLINK (NLINK, 2) = QOC (NLINK, 3)
            ENDIF
100      END DO



      ENDIF
!                             IF THE SEDIMENT CODE IS NOT RUNNING, SET
!                             UP FLOWS INTO LINKS

      TSE = D0 * DTUZ / Z2SQ
!                            SET NON-DIMENSIONED TIME STEP

!SB 230925 call nitrate component
      IF (ismn) then
         CALL MNCONT(MND,MNFC,MNFN,MNPL,MNPR,MNOUT1,MNOUT2,MNOUTPL,NCETOP,NCON,NEL,NLF, &
            NS,NV,NX,NY, &
            ICMBK,ICMREF(1,5),ICMXY, &
            NCOLMB,NLYR,NRD,NVC,NLYRBT,NTSOIL, &
            D0,TIH,RHOPL,Z2, &
            DELONE,DXQQ,DYQQ,VSPOR, &
            DELTAZ,PLAI,RDF,ZVSNOD, &
            BEXBK,LINKNS, &
            DTUZ,UZNOW, &
            CLAI,CCCC,PNETTO,SSSS,TA,VSPSI,VSTHE,VSTHEO, &
            SSS1,SSS2 )
      endif


      IF (ISPLT) CALL PLPREP
!                            Prepare for plant uptake calculations
      DO 1 NDUM = 1, NEL
         NELM = ISORT (NDUM)
         IF (NELM.GT.NLF) THEN
            CALL COLMW (NELM)
            CALL COLMSM (NELM)
         ELSE
            CALL LINKW (NELM)
            CALL LINKSM (NELM)
         ENDIF


1     END DO
!                             STEP THROUGH COLUMNS AND LINKS
!                             UPDATING THE CONCENTRATIONS IN THE
!                             CATCHMENT ARRAYS CCCC AND SSSS
      DO 10 NCONT = 1, NCON
         DO 11 NELM = 1, NLF
            DO 12 NCE = NCETOP - 2, NCETOP
               CCCCO (NELM, NCE, NCONT) = CCCC (NELM, NCE, NCONT)
12          END DO

11       END DO
         DO 13 NELM = NLF + 1, NEL
!#######################################################################
            RSZWLO (NELM) = QVSWEL (NELM)
!                               put here temporarily after introduction
!                                of irrigation
!#######################################################################
            DO 14 NCE = NLYRBT (NELM, 1), NCETOP
               CCCCO (NELM, NCE, NCONT) = CCCC (NELM, NCE, NCONT)
               SSSSO (NELM, NCE, NCONT) = SSSS (NELM, NCE, NCONT)
14          END DO
13       END DO

10    END DO
!                             SAVE THE NEW CONCENTRATIONS, FOR THE
!                             ENTIRE CATCHMENT, FOR USE AT THE NEXT
!                             TIME LEVEL
   END SUBROUTINE CMSIM



!> Assembles and solves contaminant equations for one contaminant in one column.
!>
!> The routine updates concentrations between cells `NCEBOT` and `NCETOP` and
!> returns the `CCAP` and `SCAP` vectors. It builds the finite-difference
!> coefficient arrays for mobile and immobile/storage phases, including vertical
!> advection, dispersion, sorption, decay, plant uptake, stream/well/source
!> interactions, and lateral exchange, then solves the coupled column system
!> through [[slvclm]].
!>
!> The manual's contaminant fields define the main reaction/partition controls:
!> nonlinear adsorption (`CM13`), Freundlich power `GNN` (`CM43`), decay
!> constant `GGLMSO` (`CM45`), soil-region exchange coefficient `ALPHA`
!> (`CM53`), dynamic adsorption-site fraction `FADS` (`CM55`), mobile-water
!> fraction `PHIDAT` (`CM57`), diffusion coefficient (`CM59`), and dispersivity
!> (`CM61`). `COLM` uses the corresponding prepared coefficients together with
!> the current water contents, velocities, dispersion, lateral fluxes,
!> source/sink terms, surface-water terms, and boundary conditions.
!>
!> Each cell has a dynamic/mobile concentration \(C_i\) and a
!> dead-space/storage concentration \(S_i\). The storage factors linearised in
!> the coefficient assembly are
!>
!> \[
!> F_C(C_i) = \phi_i\theta_i + f_iK_{d,i}C_i^{n_F-1},
!> \qquad
!> F_S(S_i) = (1-\phi_i)\theta_i + (1-f_i)K_{d,i}S_i^{n_F-1},
!> \]
!>
!> where \(\theta\) is water content, \(\phi\) is the mobile-water fraction,
!> \(f\) is the dynamic adsorption-site fraction, \(K_d\) is the reference
!> distribution coefficient, and \(n_F\) is the Freundlich power. Thus
!> \(C_iF_C\) and \(S_iF_S\) represent dissolved plus sorbed storage in the
!> dynamic and dead-space regions. The derivatives of these factors are stored
!> in `FCAPC` and `GCAPS` and are used when nonlinear adsorption is active.
!>
!> In balance form, the assembled finite-difference equations represent the
!> dynamic-region storage change as transport plus exchange and external/source
!> terms, and the dead-space storage change as exchange plus reaction/storage
!> terms:
!>
!> \[
!> \frac{d(C_iF_C)}{dt}
!> = T_i(C) + X_i(S-C) + M_i(C) + B_i(C),
!> \]
!>
!> \[
!> \frac{d(S_iF_S)}{dt}
!> = -X_i(S-C) + A_i(S) + R_i(S),
!> \]
!>
!> where \(T_i\) collects vertical advection and dispersion, \(X_i\) is exchange
!> between dynamic and dead-space regions, \(M_i\) contains lateral flow,
!> rainfall/source/sink, surface-water and sediment terms, \(B_i\) represents
!> boundary terms, and \(A_i\), \(R_i\) include dead-space exchange, decay, and
!> reaction/source terms. `COLM` expands these balances into the coefficient
!> arrays `DLT`, `ELT`, `FLT`, `GLT`, `PLT`, `QLT`, `SLT`, and `TLT`.
!>
!> For each adjusted active-cell index \(i = NC-NCEBOT+1\), [[slvclm]] solves
!> for the timestep rates `OME(i)` and `EPS(i)`:
!>
!> \[
!> \Omega_i = \frac{dC_i}{dt},\qquad \epsilon_i = \frac{dS_i}{dt}.
!> \]
!>
!> The assembled linearised system has the block-tridiagonal form
!>
!> \[
!> FLT_i\Omega_{i-1} + ELT_i\Omega_i + DLT_i\Omega_{i+1}
!>      - GLT_i\epsilon_i = SLT_i,
!> \]
!>
!> \[
!> PLT_i\epsilon_i - TLT_i\Omega_i = QLT_i.
!> \]
!>
!> `SLVCLM` eliminates \(\epsilon_i\) from the second equation using
!>
!> \[
!> \epsilon_i = \frac{QLT_i + TLT_i\Omega_i}{PLT_i}
!> \]
!>
!> and solves the resulting tridiagonal system
!>
!> \[
!> FLT_i\Omega_{i-1}
!> + \left(ELT_i-\frac{GLT_iTLT_i}{PLT_i}\right)\Omega_i
!> + DLT_i\Omega_{i+1}
!> = SLT_i+\frac{GLT_iQLT_i}{PLT_i}.
!> \]
!>
!> When nonlinear adsorption is enabled, `ELTSTR` and `PLTSTR` add the
!> concentration-derivative terms. `SLVCLM` performs ten Picard-style
!> coefficient updates with
!>
!> \[
!> ELT_i^\* = ELT_i + ELTSTR_i\Omega_i,\qquad
!> PLT_i^\* = PLT_i + PLTSTR_i\epsilon_i,
!> \]
!>
!> then repeats the same elimination and tridiagonal solve. After the solve,
!> `COLM` maps the adjusted solver index back to the physical column cell `NC`
!> and writes the end-of-timestep concentrations:
!>
!> \[
!> CCAP_{NC} = COLCAP_{NC} + TSE\,OME_i,\qquad
!> SCAP_{NC} = SOLCAP_{NC} + TSE\,EPS_i.
!> \]
!>
!> The linearised storage/generation terms are then corrected with the solved
!> rates:
!>
!> \[
!> GNERD_{NC} \leftarrow GNERD_{NC} + WORKA_{NC}\,OME_i,\qquad
!> GNDSE_{NC} \leftarrow GNDSE_{NC} + WORKB_{NC}\,EPS_i.
!> \]
!>
!> These corrections complete the linearisation used when the storage terms
!> depend on the updated mobile or dead-space concentration.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought `IMPLICIT` declarations from `AL.P`. |
!> | 1995-05-09 | RAH | 4.0 | Incorporated `KSP` into expressions for `OCAPP` and `OCAPP1`. |
!> | 1997-03-13 | RAH | 4.1 | Added explicit typing and generic intrinsics. |
!> @endhistory
   SUBROUTINE COLM
! Commons and constants
      USE COLM_C1
      USE COLM_C2
      USE COLM_CC
      USE COLM_CC1
!
! Locals, etc
!INTRINSIC ABS, SIGN
      INTEGER :: NC, J, NCADJ, NDUM
      DOUBLEPRECISION TTHT, TTHT1, PPHITH, PPHIT1, PPHTHP, PPHTP1
      DOUBLEPRECISION KKD, MCAP, MCAPC, MCAPT, WORKA (LLEE), WORKB ( &
         LLEE)
      DOUBLEPRECISION FFKD, GGNMON, AALPH
      DOUBLEPRECISION SUM1, SUM2, SUM3, CBCAPC, OMCBCC, CBCAP, CBCAPT
      DOUBLEPRECISION ANCAP, ANCAPT, ANCAPS, BCAP, BCAP1, BCAPSG
      DOUBLEPRECISION FCAP, FCAPT, FCAPC
      DOUBLEPRECISION GCAP, GCAPT, GCAPS, GMCAP, GMCAP1, GMCPSG
      DOUBLEPRECISION OCAPM, OCAPP, OCAPM1, OCAPP1
      DOUBLEPRECISION PCAPM, PCAPP, PCAPM1, PCAPP1
      DOUBLEPRECISION BPGSG, BMGSG, DUMMY
      DOUBLEPRECISION ALT, ALT1, HLT, HLT1, BLT, BLT1, ALTSG, HLTSG, &
         BLTSG
      DOUBLEPRECISION VCAP, VCAP1
      DOUBLEPRECISION CBSWC, OMCBSC, CBSW, CBSWT, RRRB, RRRBT, RRRBC
      DOUBLEPRECISION CCPRFC, OMCRFC, CBRF, CBRFT
!
!----------------------------------------------------------------------*
!
      OCAPP = zero
      OCAPP1 = zero
      PCAPP = zero
      PCAPP1 = zero
      DO 1 NC = NCEBOT, NCETOP
!     ^^^^^^^^^^^^^^^^^^^^^^ MAIN LOOP - SETS ELEMENTS, FOR ALL
!                                        CELLS, FOR VECTORS FOR
!                                        DIFFERENCE EQUATIONS
         TTHT = TTHET (NC)
         TTHT1 = TTHET1 (NC)
         PPHITH = PPHI (NC) * TTHT
         PPHIT1 = PPHI1 (NC) * TTHT1
         PPHTHP = PPHI (NC + 1) * TTHET (NC + 1)
         PPHTP1 = PPHI1 (NC + 1) * TTHET1 (NC + 1)
         KKD = KKDSO (NC)
         FFKD = FFSO (NC) * KKD
         GGNMON = GGNNSO (NC) - one


         AALPH = AALPSO (NC)
!                            SET DEPTH AND SOIL DEPENDENT
!                            VARIABLES
         SUM1 = zero
         SUM2 = zero
         SUM3 = zero
         IF (NC.LE. (NCEPSF + 1) ) THEN
            DO 2 J = 1, 4
               CBCAPC = half - SIGN (half, QQ (NC, J) )
               OMCBCC = one - CBCAPC
               CBCAP = OMCBCC * CCAPA (NC, J) + CBCAPC * COLCAP (NC)
               CBCAPT = OMCBCC * CCAPAT (NC, J)
               SUM1 = SUM1 + QQ (NC, J) * CBCAP
               SUM2 = SUM2 + (QQ1 (NC, J) / ZONE1 - QQ (NC, J) / ZONE) &
                  * CBCAP + TSE * QQ (NC, J) * CBCAPT / ZONE
               SUM3 = SUM3 + QQ (NC, J) * CBCAPC
2           END DO
         ENDIF
!                            SUM CONVECTION TERMS OVER FOUR FACES
         MCAP = GNERD (NC) - EDCAP (NC) + CST1 * SUM1
         MCAPT = GND2 (NC) - EDCAPT (NC) + CST1 * SUM2 * ZONE / TSE
         MCAPC = - EDCAPC (NC) + CST1 * SUM3
         ANCAP = GNDSE (NC) - ESCAP (NC)
         ANCAPT = GNDSE2 (NC) - ESCAPT (NC)
         ANCAPS = - ESCAPS (NC)
         BCAP = Z2SQOD * (AALPH + half * ABS (GGAMM (NC) ) )
         BCAP1 = Z2SQOD * (AALPH + half * ABS (GGAMM1 (NC) ) )
         BCAPSG = OMSGMA * BCAP + SGMA * BCAP1
         FCAP = PPHITH + FFKD * COLCAP (NC) **GGNMON
         FCAPT = (PPHIT1 - PPHITH) / TSE
         FCAPC = GGNMON * (FCAP - PPHITH) / COLCAP (NC)
         GND2 (NC) = GCAPLA * FCAPT * COLCAP (NC)
         GNERD (NC) = GCAPLA * COLCAP (NC) * FCAP + SGTSE * GND2 (NC)
         WORKA (NC) = GCAPLA * FCAP * SGTSE
!                            SET GENERATION TERMS FOR DYNAMIC REGION
!                            A FURTHER TERM WILL BE ADDED TO GENRD LATER
         GCAP = TTHT - PPHITH + (KKD-FFKD) * SOLCAP (NC) **GGNMON
         GCAPT = (TTHT1 - PPHIT1 - TTHT + PPHITH) / TSE
         GCAPS = GGNMON * (GCAP - TTHT + PPHITH) / SOLCAP (NC)
         GNDSE2 (NC) = GCAPLA * GCAPT * SOLCAP (NC)
         GNDSE (NC) = GCAPLA * SOLCAP (NC) * GCAP + SGTSE * GNDSE2 (NC)
         WORKB (NC) = GCAPLA * GCAP * SGTSE
!                            SET GENERATION TERMS FOR DEAD SPACE
!                            A FURTHER TERM WILL BE ADDED TO GNDSE LATER
         GMCAP = Z2SQOD * GGAMM (NC) / two
         GMCAP1 = Z2SQOD * GGAMM1 (NC) / two
         GMCPSG = OMSGMA * GMCAP + SGMA * GMCAP1
         OCAPM = OCAPP
         OCAPP = two * PPHITH * DDOD (NC) * PPHTHP * DDOD (NC + 1) &
            * KSP (NC) * KSP (NC + 1) / (PPHITH * DDOD (NC) * KSP (NC + 1) &
            + PPHTHP * DDOD (NC + 1) * KSP (NC) )
!                            WEIGHTED HARMONIC MEAN
         OCAPM1 = OCAPP1
         OCAPP1 = two * PPHIT1 * DDOD1 (NC) * PPHTP1 * DDOD1 (NC + 1) &
            * KSP (NC) * KSP (NC + 1) / (PPHIT1 * DDOD1 (NC) * KSP (NC + 1) &
            + PPHTP1 * DDOD1 (NC + 1) * KSP (NC) )
         PCAPM = PCAPP
         PCAPP = Z2OD * UUAJP (NC)
         PCAPM1 = PCAPP1


         PCAPP1 = Z2OD * UUAJP1 (NC)
!                            SET VALUES FOR NON-DIMENSIONED
!                            VARIABLES
         BPGSG = BCAPSG + GMCPSG
         BMGSG = BCAPSG - GMCPSG
         DUMMY = one / KSP (NC)
         ALT = DUMMY * MAX (zero, OCAPP / KSPP (NC) - half * PCAPP, &
            - PCAPP)
         ALT1 = DUMMY * MAX (zero, OCAPP1 / KSPP (NC) - half * &
            PCAPP1, - PCAPP1)
         HLT = DUMMY * MAX (zero, OCAPM / KSPP (NC - 1) + half * &
            PCAPM, PCAPM)
         HLT1 = DUMMY * MAX (zero, OCAPM1 / KSPP (NC - 1) + half * &
            PCAPM1, PCAPM1)
         BLT = - ALT - HLT - DUMMY * (PCAPP - PCAPM)
         BLT1 = - ALT1 - HLT1 - DUMMY * (PCAPP1 - PCAPM1)
         ALTSG = OMSGMA * ALT + SGMA * ALT1
         HLTSG = OMSGMA * HLT + SGMA * HLT1


         BLTSG = OMSGMA * BLT + SGMA * BLT1
!                            SET WORKING VALUES, AND
!                            COEFFICIENTS (A, B, AND H) FOR COMBINED
!                            CONVECTION AND DDERSION TERM


         NCADJ = NC - NCEBOT + 1
!                            ADJUST CELL NUMBERS SO THE COEFFICIENTS
!                            BELOW ARE SET FOR NCADJ=1,2,3 ETC
         DLT (NCADJ) = - SGTSE * ALTSG
         ELT (NCADJ) = SGTSE * ( - BLTSG + BPGSG) + OPSGL * (FCAP + &
            FCAPC * COLCAP (NC) ) + OPSGSL * TSE * FCAPT - SGTSE * MCAPC
         ELTSTR (NCADJ) = OPSGSL * TSE * FCAPC
         FLT (NCADJ) = - SGTSE * HLTSG
         GLT (NCADJ) = SGTSE * BMGSG
         PLT (NCADJ) = SGTSE * BMGSG + OPSGL * (GCAP + GCAPS * SOLCAP ( &
            NC) ) + OPSGSL * TSE * GCAPT - SGTSE * ANCAPS
         PLTSTR (NCADJ) = OPSGSL * TSE * GCAPS
         QLT (NCADJ) = - (GCAPLA * GCAP + BMGSG + OPSGL * GCAPT) &
            * SOLCAP (NC) + BPGSG * COLCAP (NC) + ANCAP + SGTSE * ANCAPT
         SLT (NCADJ) = ALTSG * COLCAP (NC + 1) + (BLTSG - BPGSG - &
            GCAPLA * FCAP - OPSGL * FCAPT) * COLCAP (NC) + HLTSG * COLCAP ( &
            NC - 1) + BMGSG * SOLCAP (NC) + MCAP + SGTSE * MCAPT


         TLT (NCADJ) = SGTSE * BPGSG
!                            SET ELEMENTS, FOR INTERNAL CELLS,
!                            OF THE VECTORS FOR THE DIFFERENCE
!                            EQUATIONS



1     END DO
!     ^^^^^^^^^^^^^^^^^^^^^^ END OF MAIN LOOP
      NC = NCETOP
      VCAP = GGGNU * Z2OD
      VCAP1 = GGGNU1 * Z2OD
      SUM1 = zero
      SUM2 = zero
      SUM3 = zero
      DO 3 J = 1, 4
         CBSWC = half - SIGN (half, QQQSW (J) )
         OMCBSC = one - CBSWC
         CBSW = OMCBSC * CSWA (J) + CBSWC * COLCAP (NCETOP)
         CBSWT = OMCBSC * CSWAT (J)
         RRRB = OMCBSC * RRRSWA (J) + CBSWC * RRRSW
         RRRBT = OMCBSC * RRRSAT (J) + CBSWC * RRRSWT
         RRRBC = CBSWC * RRRSWC
         SUM1 = SUM1 + QQQSW (J) * RRRB * CBSW
         SUM2 = SUM2 + (QQQSW1 (J) - QQQSW (J) ) * RRRB * CBSW + QQQSW ( &
            J) * TSE * (RRRB * CBSWT + RRRBT * CBSW)
         SUM3 = SUM3 + QQQSW (J) * (RRRB * CBSWC + RRRBC * CBSW)
3     END DO
!                            SUM CONVECTION TERMS OVER FOUR FACES
      MCAP = MCAP + (VCAP * (FCAP * COLCAP (NC) + GCAP * SOLCAP (NC) ) &
         - ESSCAP - ICAP - QCAP + CST2 * SUM1) / KSP (NC)
!                            THE GENERATION TERM FOR SOIL,
!                            SURFACE WATER, AND SEDIMENTS IS INCLUDED
!                            IN MCAP AS SET IN THE MAIN LOOP
      MCAPT = MCAPT + ( (VCAP1 - VCAP) * (FCAP * COLCAP (NC) + GCAP * &
         SOLCAP (NC) ) / TSE+VCAP * (FCAPT * COLCAP (NC) + GCAPT * SOLCAP ( &
         NC) ) - ESSCPT - ICAPT - QCAPT + CST2 * SUM2 / TSE) / KSP (NC)
      MCAPC = MCAPC + (VCAP * (FCAPC * COLCAP (NC) + FCAP) - ESSCPC - &
         ICAPC - QCAPC + CST2 * SUM3) / KSP (NC)
!                            THE FOLLOWING CODE MUST COME AFTER
!                            MCAP IS OVERWRITTEN
      FCAP = FCAP + (DDDSW * RRRSW + DDDLS * TTTLSE * RRRLS) / (Z2 * &
         KSP (NC) )
      FCAPT = FCAPT + (RRRSW * (DDDSW1 - DDDSW) + TTTLSE * RRRLS * &
         (DDDLS1 - DDDLS) + TSE * (DDDSW * RRRSWT + DDDLS * TTTLSE * &
         RRRLST) ) / (TSE * KSP (NC) * Z2)
      FCAPC = FCAPC + (DDDSW * RRRSWC + DDDLS * TTTLSE * RRRLSC) &
         / (KSP (NC) * Z2)
!                            ADD EFFECT OF SURFACE WATER AND SED. TO F
      GND2 (NC) = GCAPLA * FCAPT * COLCAP (NC)
      GNERD (NC) = GCAPLA * COLCAP (NC) * FCAP + SGTSE * GND2 (NC)
      WORKA (NC) = GCAPLA * FCAP * SGTSE
      BLT = - HLT + DUMMY * PCAPM
      BLT1 = - HLT1 + DUMMY * PCAPM1
      BLTSG = OMSGMA * BLT + SGMA * BLT1
      NCADJ = NC - NCEBOT + 1
      DLT (NCADJ) = zero
      ELT (NCADJ) = SGTSE * ( - BLTSG + BPGSG) + OPSGL * (FCAP + FCAPC * &
         COLCAP (NC) ) + OPSGSL * TSE * FCAPT - SGTSE * MCAPC
      ELTSTR (NCADJ) = OPSGSL * TSE * FCAPC


      SLT (NCADJ) = (BLTSG - BPGSG - GCAPLA * FCAP - OPSGL * FCAPT) &
         * COLCAP (NC) + HLTSG * COLCAP (NC - 1) + BMGSG * SOLCAP (NC) &
         + MCAP + SGTSE * MCAPT
!                            OVERWRITE VECTOR ELEMENTS
!                            FOR THE TOP CELL
      NC = NCEBOT
      IF (ISFLXB) THEN
         CCPRFC = half - SIGN (half, QQRF)
         OMCRFC = one - CCPRFC
         CBRF = OMCRFC * CCPRF + CCPRFC * COLCAP (NC)
         CBRFT = OMCRFC * CCPRFT
         ELT (1) = ELT (1) - CST3 * SGTSE * QQRF * CCPRFC
         SLT (1) = SLT (1) + CST3 * QQRF * CBRF
         SLT (1) = SLT (1) + CST3 * SGTSE * ( (QQRF1 - QQRF) * CBRF + &
            TSE * QQRF * CBRFT) / TSE
      ELSE
         DLT (1) = zero
         ELT (1) = one
         ELTSTR (1) = zero
         FLT (1) = zero
         GLT (1) = zero
         SLT (1) = (CCAP (NCEBOT) - COLCAP (NCEBOT) ) / TSE


      ENDIF
!                            OVERWRITE VECTOR ELEMENTS
!                            FOR THE BOTTOM CELL
      NDUM = NCETOP - NCEBOT + 1

      CALL SLVCLM (NDUM)
      DO 4 NC = NCEBOT, NCETOP
         NCADJ = NC - NCEBOT + 1
         CCAP (NC) = COLCAP (NC) + OME (NCADJ) * TSE
         SCAP (NC) = SOLCAP (NC) + EPS (NCADJ) * TSE
         GNERD (NC) = GNERD (NC) + WORKA (NC) * OME (NCADJ)
         GNDSE (NC) = GNDSE (NC) + WORKB (NC) * EPS (NCADJ)

4     END DO
!                            SET ELEMENTS OF CONCENTRATION VECTORS
!                            AND GENERATION VECTORS
   END SUBROUTINE COLM



!> Updates all contaminant concentrations for one land column.
!>
!> `COLMSM` gathers sediment, surface-water, plant, bank/link, and soil-column
!> state for column `NCL`, prepares retardation and exchange terms for each
!> contaminant, invokes [[plcolm]] and [[colm]], and writes updated dissolved,
!> sorbed, sediment, and plant-related concentrations back to shared arrays.
!>
!> For each contaminant, the routine copies the previous mobile and dead-space
!> column concentrations from `CCCCO` and `SSSSO` into `COLCAP` and `SOLCAP`.
!> It then prepares the cell-wise soil transport and partition controls used by
!> [[colm]]:
!>
!> \[
!> D_i = OODO\,DISP(NCONT,JSOL_i,\theta_i,U_{i-1},U_i),
!> \]
!>
!> together with the soil exchange coefficient `ALPHA`, adsorption-site
!> fraction `FADS`, Freundlich power `GNN`, and soil distribution coefficient
!> `KDDSOL`.
!>
!> Lateral coupling is prepared face by face. Internal faces use a flow-weighted
!> adjacent-column concentration,
!>
!> \[
!> C_{adj} =
!> \frac{\sum q_{adj} C_{adj}}{\sum q_{adj}},
!> \]
!>
!> when the summed flow is nonzero; catchment-boundary faces use the external
!> boundary concentration `CCAPE`; exposed bank faces use bank or stream
!> concentrations depending on depth. The surface concentration derivative terms
!> are also prepared here for implicit lateral coupling.
!>
!> | Face case | Subsurface concentration source | Surface concentration source |
!> |:----------|:-------------------------------|:-----------------------------|
!> | Internal land face | Flow-weighted old concentrations from `NWORK(face)`. | Adjacent surface concentration and `RSW` terms. |
!> | Catchment boundary | External concentration `CCAPE(NCL,NCONT)`. | `CCAPE`; no sediment is carried over the boundary. |
!> | Exposed bank face | Bank column below the bed; link water above it. | Adjacent stream concentration and `FSF` terms. |
!>
!> Ground-surface and surface-water retardation factors are calculated with
!> [[ret]] from sediment fractions and `KDDLS`. These provide `RRRLS` for loose
!> sediment and `RRRSW` for surface water, plus their concentration and time
!> derivatives. Surface contaminant input is assembled from rainfall, wells, and
!> optional boundary/reservoir terms:
!>
!> \[
!> QCAP = \frac{Z2}{DDA\,DDB}\left[(QI-QQQWEL)C_I
!>        + QQQWEL\,C_{well}\right],
!> \]
!>
!> with the corresponding time derivative stored in `QCAPT`.
!>
!> Bank elements add two extra source terms. `CDUM` stores the flow-rate averaged
!> concentration of water moving from bank to stream in global concentration
!> element 1, and `DUMBED` represents bed-exchange contaminant transport added
!> to `EDCAP` at `NCEBD+1`. These terms are zero for non-bank columns.
!>
!> If the plant component is active, [[plcolm]] supplies plant/source terms
!> (`EDCAP`, `ESCAP`, and derivatives). If the nitrate component is active,
!> these source terms are replaced by the MN dynamic/dead-space source arrays
!> `SSS1` and `SSS2`; for the nitrate contaminant, direct surface additions are
!> suppressed because they are already represented in MN.
!>
!> After [[colm]] solves the column equations, `COLMSM` writes the updated
!> concentrations back to the shared arrays with a small positive floor:
!>
!> \[
!> CCCC(NCL,i,NCONT)=\max(10^{-16},CCAP_i),\qquad
!> SSSS(NCL,i,NCONT)=\max(10^{-16},SCAP_i).
!> \]
!>
!> For bank columns it also stores dynamic and dead-space storage factors for
!> later bank/link erosion calculations. With nonlinear adsorption disabled,
!>
!> \[
!> F_{bk}=\phi\theta+fK_d,\qquad
!> G_{bk}=(1-\phi)\theta+(1-f)K_d,
!> \]
!>
!> and with nonlinear adsorption enabled the adsorption terms are multiplied by
!> `COLCAP**(GNN-1)` and `SOLCAP**(GNN-1)`, respectively.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-09-30 | GP | 3.4 | Initialised `EDCAP*` and `ESCAP*` when the plant option is inactive. |
!> | 1994-10-03 | RAH | 3.4.1 | Brought `IMPLICIT` declarations from `AL.P`. |
!> | 1996-07-17 | GP | 4.0 | Revised lateral-flow averaging and incorporated well flow in `QCAP`/`QCAPT`. |
!> | 1997-03-14 | RAH | 4.1 | Added explicit typing, split mixed-type `/WTOC/`, and used generic intrinsics. |
!> | 1997-05-21 | RAH | 4.1 | Removed redundant `JFACEA`, `NAQU`, and `TRAN` workspace. |
!> @endhistory
   SUBROUTINE COLMSM (NCL)

! Commons and constants
      USE SED_CS
      USE CONT_CC
      USE COLM_C1
      USE COLM_C2
      USE COLM_CC
      USE COLM_CG
      USE BK_CW
      USE SED_CO

      USE PLANT_CC
! Input common
!INTEGER :: JBK, JFLINK, JSOL (LLEE), NWORK (4), NLINKA, NCWELL
!DOUBLEPRECISION VELDUM (LLEE), QQQWEL, QQQWL1, QQRV (LLEE), &
! ROH (LLEE)
!LOGICAL :: ISBDY (4)
!COMMON / WTOCI / JBK, JFLINK, JSOL, NWORK, NLINKA, NCWELL
!COMMON / WTOC / VELDUM, QQQWEL, QQQWL1, QQRV, ROH

!COMMON / WTOCL / ISBDY
!                             VARIABLES USED ONLY IN COLMW AND COLMSM
      INTEGER, INTENT(IN) :: NCL !! Land-column element being updated.
! Locals, etc
!INTRINSIC ABS, MAX, MIN
      INTEGER :: NCONT, NCE, JA, NDUM, NOLDUM, NOLP, JCEA, JSED
      DOUBLEPRECISION CCBT, SUM, SUMQ, SUMQC, SUMW
      DOUBLEPRECISION :: DUM, DUM0, DUM1, DUM2, DUM3, DUMBED, CDUM=0.0d0
      DOUBLEPRECISION GNDUM, QDUM, QCDUM, QCDUM1, UDUMP, UDUMM, UCDUMP, &
         UCDUMM

      DOUBLEPRECISION FBO (NSEDEE), FB (NSEDEE), FDLO (NSEDEE), FDL ( &
         NSEDEE), KDDUM (NSEDEE)
!----------------------------------------------------------------------*
!
      DO 1 NCE = 1, LLEE
         GNERD (NCE) = zero
         GNDSE (NCE) = zero
         GND2 (NCE) = zero
         GNDSE2 (NCE) = zero



1     END DO
!                             SET GENERATION VARIABLES TO ZERO IN
!                             PREPARATION FOR THE 1ST PASS OF DO LOOP 5

      DO 5 NCONT = 1, NCON
!                             +++++ MAIN LOOP FOR UPDATING CONCS ++++++
         DO 6 NCE = NCEBOT - 1, NCETOP + 1
            COLCAP (NCE) = CCCCO (NCL, NCE, NCONT)
            SOLCAP (NCE) = SSSSO (NCL, NCE, NCONT)

6        END DO
!                             SET OLD CONCENTRATION VECTORS

         GCAPLA = GCPLA (NCONT)
         DO 11 NCE = NCEBOT, NCETOP
            DDOD (NCE) = OODO * DISP (NCONT, JSOL (NCE), TTHET (NCE), &
               UUAJP (NCE-1), UUAJP (NCE) )
            DDOD1 (NCE) = OODO * DISP (NCONT, JSOL (NCE), TTHET1 (NCE), &
               UUAJP1 (NCE-1), UUAJP1 (NCE) )
            AALPSO (NCE) = ALPHA (JSOL (NCE), NCONT)
            FFSO (NCE) = FADS (JSOL (NCE), NCONT)
            GGNNSO (NCE) = GNN (NCONT)
            KKDSO (NCE) = KDDSOL (JSOL (NCE), NCONT)
11       END DO
         DDOD (NCETOP + 1) = zero

         DDOD1 (NCETOP + 1) = zero
!                            SET THE EFFECTIVE DISPERSION COEFFICIENTS
!                            AND OTHER SOIL PROPERTIES

         DO 12 JA = 1, 4

            IF (.NOT.ISBDY (JA) .AND. (JA.NE.JFLINK) ) THEN
!                             IS NOT FACE AT CATCHMENT BOUNDARY OR THE
!                             EXPOSED FACE OF A BANK
               NDUM = NCEPSF + 1
               DO 13 NCE = NCEBOT, MIN (NDUM, NCETOP)
                  SUMQ = zero
                  SUMQC = zero
                  NOLDUM = MAX (1, NOLBT (NCL, NCE, JA) )
                  DO 14 NOLP = NOLDUM, NOLBT (NCL, NCE+1, JA) - 1
                     JCEA = NOLCEA (NCL, NOLP, JA)
                     QDUM = QQ1 (NCE, JA)
                     SUMQ = SUMQ + QDUM
                     SUMQC = SUMQC + QDUM * CCCCO (NWORK (JA), JCEA, &
                        NCONT)
14                END DO
                  IF (NOTZERO(SUMQ)) SUMQ = SUMQC / SUMQ
                  CCAPA (NCE, JA) = SUMQ
                  CCAPAT (NCE, JA) = zero
13             END DO
!                             EXPLICIT (IN C) LATERAL COUPLING IN
!                             SUBSURFACE
               CSWA (JA) = CCCCO (NWORK (JA), NCETOP, NCONT)
               CSWAT (JA) = (CCCC (NWORK (JA), NCETOP, NCONT) - CSWA ( &
                  JA) ) / TSE
               RRRSWA (JA) = RSW (NWORK (JA), NCONT)

               RRRSAT (JA) = RSWT (NWORK (JA), NCONT) + RSWC (NWORK (JA) &
                  , NCONT) * CSWAT (JA)
!                             IMPLICIT (IN C) LATERAL COUPLING IN SURF.
!                             NB: TIME DERIVATIVE OF R IN ADJACENT
!                             COLUMN INCLUDES THE EFFECT OF THE CHANGING
!                             CONC. IN THAT COLUMN

            ELSEIF (ISBDY (JA) ) THEN
!                             IF ADJACENT COLUMN IS OUTSIDE BOUNDARY
               DO 16 NCE = NCEBOT, NCEPSF + 1
                  CCAPA (NCE, JA) = CCAPE (NCL, NCONT)
                  CCAPAT (NCE, JA) = zero
16             END DO
               CSWA (JA) = CCAPE (NCL, NCONT)
               CSWAT (JA) = zero
               RRRSWA (JA) = one
!                             NB: NO SEDIMENT WITH FLOWS OVER BOUNDARY

               RRRSAT (JA) = zero

            ELSE
!                             IS THE EXPOSED FACE OF A BANK COLUMN
               DO 19 NCE = NCEBOT, NHBED (NLINKA, JBK)
                  CCAPA (NCE, JA) = CCCCO (NWORK (JA), NCE, NCONT)
                  CCAPAT (NCE, JA) = zero
19             END DO
!                             EXPLICIT (IN C) LATERAL COUPLING IN
!                             SUBSURFACE
               DO 17 NCE = NHBED (NLINKA, JBK) + 1, NCETOP
                  CCAPA (NCE, JA) = CCCCO (NLINKA, NCETOP, NCONT)
                  CCAPAT (NCE, JA) = (CCCC (NLINKA, NCETOP, NCONT) &
                     - CCAPA (NCE, JA) ) / TSE
!                             IMPLICIT COUPLING WITH STREAM WATER FOR
!                             SUBSURFACE EXPOSED BANK CELLS
17             END DO
               CSWA (JA) = CCAPA (NCETOP, JA)
               CSWAT (JA) = CCAPAT (NCETOP, JA)
               RRRSWA (JA) = FSF (NLINKA, NCONT)
               RRRSAT (JA) = FSFT (NLINKA, NCONT) + FSFC (NLINKA, NCONT) &
                  * CSWAT (JA)
!                             NB: TIME DERIVATIVE OF F IN ADJACENT
!                             LINK INCLUDES THE EFFECT OF THE CHANGING
!                             CONC. IN THAT LINK

            ENDIF


12       END DO
!                             SET CONCENTRATIONS AND RETARDATION FACTORS
!                             IN ADJACENT COLUMN
         IF (.NOT.ISFLXB) THEN
            CCBT = CCAPB (NCL, NCONT)
            CCAP (NCEBOT) = CCBT
!                             NB: CCAP(NCEBOT) IS USED AS THE BOUNDARY
!                             CONCENTRATION IN SUBROUTINE COLM
            CCPRF = zero
            CCPRFT = zero
         ELSE
            CCPRF = CCAPR (NCL, NCONT)
            CCPRFT = zero
            CCBT = CCPRF
         ENDIF
         DO 20 NCE = 1, NCEBOT - 1
            COLCAP (NCE) = CCBT
            CCAP (NCE) = CCBT
            SCAP (NCE) = CCBT



20       END DO
!                            SET BOTTOM CELL VARIABLES
         DO 22 JSED = 1, NSED
            KDDUM (JSED) = KDDLS (JSED, NCONT)
            FBO (JSED) = FBETAO (NCL, JSED)
            FB (JSED) = FBETA (NCL, JSED)
            FBETAO (NCL, JSED) = FB (JSED)
            FDLO (JSED) = FDELO (NCL, JSED)
            FDL (JSED) = FDEL (NCL, JSED)
            FDELO (NCL, JSED) = FDL (JSED)

22       END DO
!                             SET UP ARRAYS FOR USE IN CALLS TO FUNCTION
!                             RET

         CALL RET (COLCAP (NCETOP), GNN (NCONT), TTTLSE, TTTLSE, FBO, &
            FB, KDDUM, RRRLS, RRRLSC, RRRLST, TSE, NSED, ISADNL)
!                             SET LOOSE SEDIMENT REATRDATION VARIABLES

         CALL RET (COLCAP (NCETOP), GNN (NCONT), one, one, FDLO, &
            FDL, KDDUM, RRRSW, RRRSWC, RRRSWT, TSE, NSED, ISADNL)
!                             SET SURFACE WATER RETARDATION VARIABLES
         RSW (NCL, NCONT) = RRRSW
         RSWC (NCL, NCONT) = RRRSWC


         RSWT (NCL, NCONT) = RRRSWT
!                             SAVE SURFACE WATER REATRDATION VALUES
!                             FOR USE IN CALCULATING LATERAL CONVECTION
!                             RATES
         ICAP = - Z2OD * IIICFO (NCONT)
         IIICFO (NCONT) = IIICF (NCONT)
         ICAPT = zero
         ICAPC = zero
         DUM = Z2OD / (DDA * DDB)
         QCDUM = (QI - QQQWEL) * CCAPIO (NCONT)
         QCDUM1 = (QI1 - QQQWL1) * CCAPI (NCONT)
         IF (NCWELL.GT.0) THEN
            QCDUM = QCDUM + QQQWEL * CCCCW (NCWELL, NCONT)
            QCDUM1 = QCDUM1 + QQQWL1 * CCCCW (NCWELL, NCONT)
         ENDIF
         QCAP = DUM * QCDUM
         QCAPT = (DUM * QCDUM1 - QCAP) / TSE

         CCAPIO (NCONT) = CCAPI (NCONT)



         QCAPC = zero
!                            SET SURFACE INPUT VARIABLES
         DO 30 NCE = NCEBOT, NCETOP
            DUMMY (NCE) = zero
30       END DO

         IF (ISBK) THEN
            SUM = zero
            SUMQ = zero
            DUM0 = Z2OD / AREA (NCL)
            DO 32 NCE = NCEAB (NLINKA, JBK), NHBED (NLINKA, JBK) &
               + 1
               SUMQ = SUMQ + QQRV (NCE)
               DUM1 = ABS (QQRV (NCE) )
               DUM2 = half * (QQRV (NCE) + DUM1)
               DUM3 = half * (QQRV (NCE) - DUM1)
               QCDUM = DUM2 * CCCC (NLINKA, NCETOP - 2, NCONT) + DUM3 * &
                  CCCCO (NCL, NCE, NCONT)
!                             IMPLICIT COUPLING TO DEEP BED CONC.
               SUM = SUM + QCDUM
               DUMMY (NCE) = DUMMY (NCE) + ROH (NCE) * QCDUM * DUM0 / &
                  KSP (NCE)

32          END DO
!                             SET SOURCE FOR CONVECTION INTO STREAM FROM
!                             BANK
            IF (NOTZERO(SUMQ)) SUMQ = SUM / SUMQ
            CDUM = SUMQ
!                             SET EFFECTIVE CONCENTRATION IN WATER FLOW
!                             INTO STREAM FROM BANK
            NCE = NCEBD (NLINKA, JBK) + 1
            UDUMP = UUAJP1 (NCE)
            UDUMM = UUAJP1 (NCE-1)
            UCDUMP = MAX (zero, UDUMP * COLCAP (NCE) ) - MAX (zero, &
               - UDUMP * COLCAP (NCE+1) )
            UCDUMM = MAX (zero, UDUMM * COLCAP (NCE-1) ) - MAX (zero, &
               - UDUMM * COLCAP (NCE) )
            DUMBED = (ROH (NCE) * VELDUM (NCE-1) - one) * UCDUMM - &
               (ROH (NCE) * VELDUM (NCE) - one) * UCDUMP

            DUMBED = Z2OD * DUMBED / KSP (NCE)

         ELSE

            DUMBED = zero

         ENDIF
         IF (ISPLT) THEN
            CALL PLCOLM (NCL, NCONT)
         ELSE
            DO 33 NCE = NCEBOT, NCETOP
               EDCAP (NCE) = zero
               EDCAPC (NCE) = zero
               EDCAPT (NCE) = zero
               ESCAP (NCE) = zero
               ESCAPS (NCE) = zero
               ESCAPT (NCE) = zero
33          END DO

         ENDIF

! SB 230925 change source terms if nitrate component being used
         if (ISMN) then
            DO NCE = NCEBOT,NCETOP
               EDCAP(NCE) = SSS1(NCL,NCE,NCONT)
               ESCAP(NCE) = SSS2(NCL,NCE,NCONT)
               EDCAPT(NCE) = 0.0D0
               EDCAPC(NCE) = 0.0D0
               ESCAPT(NCE) = 0.0D0
               ESCAPS(NCE) = 0.0D0
            END DO
!
!       *  The first contaminant is nitrate and surface additions
!       *  are considered in the MN component
            IF (NCONT.EQ.1) THEN
               ICAP = 0.0D0
               QCAP = 0.0D0
               QCAPT = 0.0D0
            ENDIF
         ENDIF


!                 Call contaminant plant uptake routine
!                  Sets EDCAP, ESCAP etc
         SUM = zero
         SUMW = zero

         DO 34 NCE = NCEBOT, NCETOP

            EDCAP (NCE) = EDCAP (NCE) - DUMMY (NCE) + WELDRA (NCE) &
               * Z2OD * COLCAP (NCE) / KSP (NCE)
!                         Add  stream and well uptake to plant uptake
            SUM = SUM + WELDRA (NCE) * COLCAP (NCE)
            SUMW = SUMW + WELDRA (NCE)
34       END DO
         IF (ISBK) THEN
            NCE = NCEBD (NLINKA, JBK) + 1
            EDCAP (NCE) = EDCAP (NCE) - DUMBED

         ENDIF
!                       Add  uptake to dry streams to plant and
!                       well uptake
         IF (NOTZERO(SUMW)) THEN
            CCCCW (NCL, NCONT) = SUM / SUMW
         ELSE
            CCCCW (NCL, NCONT) = zero



         ENDIF
!                             SET PLANT WELL AND STREAM UPTAKE
!                             VARIABLES; AND SET THE MIXED WELL WATER
!                             CONCENTRATION FOR USE IN PRINTOUTS
!                             NB: WELL UPTAKE AND LOSS TO STREAM VIA BED
!                             INCLUDED IN EDCAP
         OPSGL = one + SGTSE * GCAPLA

         OPSGSL = one + SGSTSE * GCAPLA
!                            SET FACTORS AND TERMS DEPENDING ON SIGMA

         CALL COLM
!                            RETURNS UPDATED CONCENTRATIONS
!                            IN THE VECTORS CCAP AND SCAP
         CCCCO (NCL, 1, NCONT) = CDUM
         CCCC (NCL, 1, NCONT) = CDUM
!                             FLOW RATE AVERAGED CONC. IN WATER FLOW
!                             FROM BANK TO STREAM STORED AS ELEMENT 1
!                             IN GLOBAL CONTAMINANT ARRAYS
         DO 40 NCE = 1, NCETOP
!    ##########################temporary MAX######################
            CCCC (NCL, NCE, NCONT) = MAX (1D-16, CCAP (NCE) )
            SSSS (NCL, NCE, NCONT) = MAX (1D-16, SCAP (NCE) )
!    ##############################################################


40       END DO
!                             SAVE THE UPDATED CONCENTRATIONS
         IF (ISBK.AND. (.NOT.ISADNL) ) THEN
            DO 42 NCE = NHBED (NLINKA, JBK) + 1, NCETOP
               FCPBKO (NLINKA, JBK, NCE, NCONT) = PPHI (NCE) * TTHET ( &
                  NCE) + FFSO (NCE) * KKDSO (NCE)
               GCPBKO (NLINKA, JBK, NCE, NCONT) = (one - PPHI (NCE) ) &
                  * TTHET (NCE) + (one - FFSO (NCE) ) * KKDSO (NCE)
42          END DO
         ELSEIF (ISBK.AND.ISADNL) THEN
            GNDUM = GNN (NCONT) - one
            DO 44 NCE = NHBED (NLINKA, JBK) + 1, NCETOP
               FCPBKO (NLINKA, JBK, NCE, NCONT) = PPHI (NCE) * TTHET ( &
                  NCE) + FFSO (NCE) * KKDSO (NCE) * COLCAP (NCE) **GNDUM
               GCPBKO (NLINKA, JBK, NCE, NCONT) = (one - PPHI (NCE) ) &
                  * TTHET (NCE) + (one - FFSO (NCE) ) * KKDSO (NCE) &
                  * SOLCAP (NCE) **GNDUM
44          END DO


         ENDIF
!                             FCPBK AND GCPBK ARE USED IN THE BANK
!                             EROSION CALCULATIONS IN LINK

5     END DO
!                             ++++++++++++ END OF MAIN LOOP +++++++++++
   END SUBROUTINE COLMSM





!> Prepares water-flow geometry and flux terms for a contaminant column solve.
!>
!> The routine maps grid, bank, link, well, vertical, and lateral water-flow
!> quantities onto the local column arrays used by [[colmsm]] and [[colm]],
!> including mobile-water fractions, old/new water contents, velocity terms, and
!> face boundary flags.
!>
!> It imports the column geometry, layer/soil mappings, old/new VSS water
!> contents and vertical flows, lateral subsurface and surface-water flows,
!> rainfall, ET, wells, bank/link geometry, sediment depths, erosion rates, and
!> plant water uptake from the shared `COLM`, `AL`, `BK`, `SED`, and `PLANT`
!> state. It updates the saved old-state arrays (`VSTHEO`, `UUAJPO`,
!> `GGAMMO`, `QQO`, `QQQSWO`, `QIO`, `QQRFO`, `DSWO`, `ZONEO`, `DLSO`,
!> `GNUO`) and fills the local arrays consumed by [[colmsm]] and [[colm]]:
!> `KSP`, `KSPP`, `TTHET`, `TTHET1`, `UUAJP`, `UUAJP1`, `PPHI`, `PPHI1`,
!> `GGAMM`, `GGAMM1`, `QQ`, `QQ1`, `QQQSW`, `QQQSW1`, `WELDRA`, `ISBDY`,
!> and `NWORK`.
!>
!> The sigma-time constants used in the later finite-difference assembly are
!>
!> \[
!> SGTSE = SGMA\,TSE,\qquad SGSTSE = SGSQ\,TSE .
!> \]
!>
!> Cell geometry is non-dimensionalised with the reference depth `Z2`:
!>
!> \[
!> KSP_i = \Delta z_i/Z2,\qquad
!> KSPP_i = (z_{i+1}-z_i)/Z2,\qquad
!> ZONE = (ZGRUND-ZCOLMB)/Z2 .
!> \]
!>
!> For bank elements, the bank column width is adjusted relative to the adjacent
!> stream half-width. With bank width \(D_{bk}=AREA/CLENTH\),
!>
!> \[
!> ROH = \frac{D_{bk}}{D_{bk}+0.5\,CWIDTH},\qquad VELDUM = 1/ROH,
!> \]
!>
!> with a partial-cell correction at the effective bed. Non-bank columns use
!> `ROH = VELDUM = 1`.
!>
!> For ordinary grid cells, the new water content and vertical flow are copied
!> from `VSTHE` and `QVSV`. For bank cells that overlap the adjacent link, the
!> new values are width-weighted mixtures:
!>
!> \[
!> \theta_i^{n+1} = (1-ROH_i)\theta_{link}^{n+1}
!>                 + ROH_i\theta_{bank}^{n+1},
!> \]
!>
!> with the same mixture for the vertical flow `UUAJP1`. The mobile-water
!> fractions are `PPHI = PHI(JSOIL,TTHET)` and
!> `PPHI1 = PHI(JSOIL,TTHET1)`. The dead-space water-change/source term is
!>
!> \[
!> GGAMM1_i =
!> \frac{(1-XXI\,\phi_i^{n+1})ROH_i\,ERUZ_i}{KSP_i Z2}
!> + \frac{(1-\phi_i^{n+1})\theta_i^{n+1}
!>       -(1-\phi_i^n)\theta_i^n}{DTUZ}.
!> \]
!>
!> Lateral subsurface face flows are scaled into local column units as
!>
!> \[
!> QQ1_{i,j} = Q1_{i,j}\,ZONE\,ROH_i/KSP_i,
!> \]
!>
!> and surface-water face flows are copied from `QOC` with the sign convention
!> used by the contaminant routines. Rain input and well/irrigation terms are
!>
!> \[
!> QI1 = -PNETTO\,AREA,\qquad
!> QQQWL1 = -QVSWEL\,AREA_{well}.
!> \]
!>
!> Surface-water face flows use this mapping:
!>
!> | Face(s) | `QQQSW1` value |
!> |:--------|:---------------|
!> | 1, 2 | `-QOC(NCL,face)` |
!> | 3, 4 | `QOC(NCL,face)` |
!>
!> The top vertical velocity is reconstructed from surface-water storage change,
!> soil evaporation, rainfall, and surface outflow:
!>
!> \[
!> U_{top}^{n+1} =
!> \frac{DSW^{n+1}-DSW^n}{Z2SQOD\,TSE}
!> + EEVAP + \frac{QI1-\sum_j QQQSW1_j}{AREA}.
!> \]
!>
!> Down-column vertical velocities are then reconstructed by a water-balance
!> recurrence including water-content change, well withdrawal, plant uptake,
!> lateral flow, and the bank `VELDUM` correction; the legacy error-smoothing
!> factor `EMULT` damps part of the correction in selected near-surface cells.
!> The base flux exported to [[colmsm]] is `QQRF1 = AREA*UUAJP1(NCEBOT-1)`.
!>
!> | Cell band from surface downward | `EMULT` |
!> |:-------------------------------|:--------|
!> | Top 5 cells | 0 |
!> | Next 3 cells | 0.1 |
!> | Next 12 cells | 0.5 |
!> | Remaining cells to `NCEBOT` | 1 |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Brought `IMPLICIT` declarations from `AL.P`; removed `INTEGER*2`. |
!> | 1996-07-17 | GP | 4.0 | Reworked bank/well/layer flow setup, bank weighting, irrigation handling, and correction damping. |
!> | 1997-02-18 | RAH | 4.1 | Swapped `QVSH`, `DELTAZ`, `ZVSNOD`, `QVSV`, `QVSWLI`, and `VSTHE` subscripts. |
!> | 1997-03 | RAH | 4.1 | Added explicit typing, split mixed-type `/WTOC/`, removed redundant locals, and condensed well code. |
!> @endhistory
   SUBROUTINE COLMW (NCL)

! Commons and constants
      USE SED_CS
      USE COLM_C1
      USE COLM_C2
      USE COLM_CO
      USE COLM_CG
      USE BK_CW
      USE SED_CO
      USE PLANT_CC
!INTEGER :: JBK, JFLINK, JSOL (LLEE), NWORK (4), NLINKA, NCWELL
!DOUBLEPRECISION VELDUM (LLEE), QQQWEL, QQQWL1, QQRV (LLEE), &
! ROH (LLEE)
!LOGICAL :: ISBDY (4)
!COMMON / WTOCI / JBK, JFLINK, JSOL, NWORK, NLINKA, NCWELL
!COMMON / WTOC / VELDUM, QQQWEL, QQQWL1, QQRV, ROH
!COMMON / WTOCL / ISBDY
!                             VARIABLES USED ONLY IN COLMW AND COLMSM
! Workspace common
!        AL.C:         DUMMY(LL)
      INTEGER, INTENT(IN) :: NCL !! Land-column element whose water-flow terms are prepared.
! Locals, etc
!INTRINSIC MAX, MOD, SIGN
      INTEGER :: JAL, JSOIL, JDUM, IW, JA, JLYR, JB
      INTEGER :: NAQU, NCE, NCEA, NCLA, NDIFF, NDUM, NELMA
      DOUBLEPRECISION DBK, DMULT, DINV, ROHDUM, OMROH, THEDUM, QVDUM, &
         PHIDUM
      DOUBLEPRECISION DUM, DUM0, DUM1, UUOLD, UUNEW, ERRDUM, UIN



      DOUBLEPRECISION Q1 (LLEE), TRAN1 (LLEE), EMULT (LLEE)
!----------------------------------------------------------------------*
! Factors & indices
!___________________*
      SGTSE = SGMA * TSE

      SGSTSE = SGSQ * TSE
!                             SET FACTORS DEPENDING ON SIGMA
      NCEBOT = NCOLMB (NCL)

      NAQU = NLYRBT (NCL, 1)
!                             SET BOTTOM COLUMN CELL, AND
!                             BOTTOM AQUIFER CELL NUMBERS
      NDUM = NCETOP - NAQU + 2
      CALL ALINIT (ONE, NDUM, ROH (NAQU - 1) )

      CALL ALINIT (ONE, NDUM, VELDUM (NAQU - 1) )
!                             set defaults
      JBK = ICMREF (NCL, 1)
      ISBK = JBK.NE.0
      IF (ISBK) THEN
!                             ELEMENT IS A BANK
         NLINKA = ICMREF (NCL, 4)
         NDIFF = NLYRBT (NLINKA, 1) - NAQU
!                             NUMBER & CELL OFFSET FOR ASSOCIATED LINK
         JAL = 0
100      JAL = JAL + 1
         IF (ICMREF (NLINKA, JAL + 4) .NE.NCL) GOTO 100
         JFLINK = ICMREF (NLINKA, JAL + 8)
!                             NUMBER FOR FACE ASSOCIATED WITH LINK
         DBK = AREA (NCL) / CLENTH (NLINKA)
         DMULT = DBK / (DBK + half * CWIDTH (NLINKA) )
         DINV = ONE / DMULT
         DO 102 NCE = NAQU - 1, NCEBD (NLINKA, JBK)
            ROH (NCE) = DMULT
            VELDUM (NCE) = DINV
102      END DO
         ROH (NCE) = ONE- (ONE-DMULT) * FNCEBD (NLINKA, JBK)
      ELSE
!                             NOT A BANK
         JFLINK = 0




      ENDIF
!                             SET ROH (& VELDUM): FOR A BANK
!                             ROH IS THE RATIO OF THE WIDTH OF THE BANK
!                             SOIL COLUMN TO THE SUM OF THE WIDTH OF THE
!                             BANK SOIL COLUMN AND HALF THE WIDTH OF THE
!                             STREAM; IT IS USED IN SUBSURFACE FLOW
!                             CALCULATIONS TO ALLOW THE SAME CODE TO BE
!                             USED FOR BANK AND NON-BANK COLUMNS
!                             NB: ROH IS 1 ABOVE THE BOTTOM OF THE BED
!                             DEEP LAYER.
!                             FOR A NON-BANK, ROH IS 1
!970521                       See also "temporary" section at the end
! Properties for each cell *
!__________________________*
      DO 190 NCE = NAQU, NCETOP
         TRAN1 (NCE) = ERUZ (NCL, NCE)

190   END DO
!                             SET LOCAL VECTOR FOR RATE OF PLANT UPTAKE
!                             OF WATER FOR THE FULL LENGTH OF THE COLUMN
      DO 221 JLYR = 1, NLYR (NCL)
         JSOIL = NTSOIL (NCL, JLYR)
         DO 212 NCE = MAX (NCEBOT, NLYRBT (NCL, JLYR) ), NLYRBT (NCL, &
            JLYR + 1) - 1
            JSOL (NCE) = JSOIL
            KSP (NCE) = DELTAZ (NCE, NCL) / Z2
            KSPP (NCE) = (ZVSNOD (NCE+1, NCL) - ZVSNOD (NCE, NCL) ) &
               / Z2
!                             NB kspp(ncetop) is overwritten below
            TTHET (NCE) = VSTHEO (NCL, NCE)

            UUAJP (NCE) = UUAJPO (NCL, NCE)
            IF (JBK.EQ.0) THEN
!                             regular column element
               TTHET1 (NCE) = VSTHE (NCE, NCL)
               UUAJP1 (NCE) = QVSV (NCE, NCL)
            ELSE
!                             element is (L-shaped) bank
!                             NB uuajp1(nhbed) is overwritten below
               NCEA = NCE+NDIFF
               IF (NCEA.LE.NCETOP) THEN
                  ROHDUM = ROH (NAQU)
                  OMROH = one - ROHDUM
                  THEDUM = VSTHE (NCEA, NLINKA)

                  QVDUM = QVSV (NCEA, NLINKA)
                  TTHET1 (NCE) = OMROH * THEDUM + ROHDUM * VSTHE (NCE, &
                     NCL)
                  UUAJP1 (NCE) = OMROH * QVDUM + ROHDUM * QVSV (NCE, &
                     NCL)
               ELSE
                  TTHET1 (NCE) = VSTHE (NCE, NCL)

                  UUAJP1 (NCE) = QVSV (NCE, NCL)

               ENDIF

            ENDIF
            VSTHEO (NCL, NCE) = TTHET1 (NCE)

            UUAJPO (NCL, NCE) = UUAJP1 (NCE)
            PHIDUM = PHI (JSOIL, TTHET1 (NCE) )
            PPHI (NCE) = PHI (JSOIL, TTHET (NCE) )
            PPHI1 (NCE) = PHIDUM
            GGAMM (NCE) = GGAMMO (NCL, NCE)
            GGAMM1 (NCE) = (one - XXI * PHIDUM) * ROH (NCE) * TRAN1 ( &
               NCE) / (KSP (NCE) * Z2) + ( (one - PHIDUM) * TTHET1 (NCE) &
               - (one - PPHI (NCE) ) * TTHET (NCE) ) / DTUZ
            GGAMMO (NCL, NCE) = GGAMM1 (NCE)
212      END DO

221   END DO
!                             ordinary cells
      KSP (NCETOP + 1) = KSP (NCETOP)
      KSPP (NCETOP) = KSP (NCETOP)

      KSPP (NCEBOT - 1) = DELTAZ (NCEBOT, NCL) / Z2
!                             special cells for KSP*
      IF (ISBK) THEN
         NCE = NHBED (NLINKA, JBK)
         UUAJP1 (NCE) = QVSV (NCE, NCL)
      ENDIF
!                             vert. vel. of cell below bed is in top
!                             part of L-shaped column (over-rides above)
      NCE = NAQU - 1
      UUAJP (NCE) = UUAJPO (NCL, NCE)
      IF (JBK.EQ.0) THEN
         UUAJP1 (NCE) = QVSV (NCE, NCL)
      ELSE
         NCEA = NCE+NDIFF
         UUAJP1 (NCE) = ( (ONE-ROH (NCE) ) * QVSV (NCEA, NLINKA) &
            + ROH (NCE) * QVSV (NCE, NCL) )
      ENDIF




      UUAJPO (NCL, NCE) = UUAJP1 (NCE)
!                             vert vel for cell below aquifer base
!                             SET cell properties, moisture content,
!                             AND VERTICAL FLOW VALUES, AND
!                             STORE 'OLD' VALUES FOR NEXT TIME STEP
!970314                       NB See "temporary code" at end of routine
! Properties common to every cell *
!_________________________________*
      TTTLSE = 1.0D-4
!                             SET MOISTURE CONTENT FOR LOOSE SEDIMENTS
      DDA = DYQQ (NCL)
      DDB = DXQQ (NCL)
      DDDSW = DSWO (NCL)
      DDDSW1 = HRF (NCL) - ZGRUND (NCL)
      DSWO (NCL) = DDDSW1
      DDDLS = DLSO (NCL)
      DDDLS1 = DLS (NCL)
      DLSO (NCL) = DLS (NCL)
      GGGNU = GNUO (NCL)
      GGGNU1 = GNU (NCL)
      GNUO (NCL) = GNU (NCL)
      ZONE = ZONEO (NCL)
      ZONE1 = (ZGRUND (NCL) - ZCOLMB (NCL) ) / Z2
      ZONEO (NCL) = ZONE1
!                             SET WIDTHS OF COLUMN,
!                             DEPTHS OF SURFACE WATER AND
!                             SEDIMENTS, EROSION RATE, AND
!                             NON-DIMENSIONED SATURATED DEPTH
      NCEPSF = NCETOP
!                             FORMERLY (pre v4.0) THE HIGHEST CELL
!                             NUMBER IN THE SATURATED ZONE;
!                             now lateral transport is allowed
!                             up to the ground surface
      CST2 = Z2 / (AREA (NCL) * D0)
      CST1 = CST2 / ZONE1



      CST3 = CST2 / KSP (NCEBOT)
!                            SET CONSTANTS USED IN CONVECTION TERMS
      DO 303 JA = 1, 4
         NELMA = ICMREF (NCL, JA + 4)
         ISBDY (JA) = NELMA.EQ.0
         IF (.NOT.ISBDY (JA) ) THEN
            IF (ICMREF (NELMA, 1) .EQ.3) THEN
               NWORK (JA) = ICMREF (NELMA, JA + 4)
            ELSE
               NWORK (JA) = NELMA
            ENDIF
         ELSE
            NWORK (JA) = NCL
!                             ASSUME MIRROR IMAGE IF FACE IS AT THE
!                             BOUNDARY OF CATCHMENT
         ENDIF



303   END DO
!                             SET NWORKj TO THE NUMBER FOR THE COLUMN
!                             ADJACENT TO FACE j
!+++++ MAIN LOOP FOR COLUMN FACES +++++*
!______________________________________*

      DO 605 JA = 1, 4
         DO 318 NCE = NCEBOT - 1, NCETOP + 1
            QQ (NCE, JA) = zero
            QQ1 (NCE, JA) = zero
            DUMMY (NCE) = zero

318      END DO
         IF (JA.EQ.JFLINK) THEN
!                             IS INSIDE FACE OF BANK
            DO 329 NCE = NCEBOT, NHBED (NLINKA, JBK)
               NCEA = NCE+NDIFF
               JB = 1 + MOD (JA + 1, 4)
               Q1 (NCE) = .5D0 * (QVSH (JA, NCEA, NLINKA) - QVSH (JB, &
                  NCEA, NLINKA) )
329         END DO
            DO 395 NCE = NHBED (NLINKA, JBK) + 1, NCETOP
               Q1 (NCE) = QVSH (JA, NCE, NCL)
395         END DO
         ELSE
!                             neighbour is a land element
            DO 410 NCE = NCEBOT, NCETOP
               Q1 (NCE) = QVSH (JA, NCE, NCL)
410         END DO
            NCLA = ICMREF (NCL, JA + 4)
            IF (ISBK.AND.NCLA.GT.0) THEN
               IF (ICMREF (NCLA, 1) .EQ.1.OR.ICMREF (NCLA, 1) .EQ.2) &
                  THEN
!                             add extra flow for end-to-end banks
                  DO 496 NCE = NCEBOT, NHBED (NLINKA, JBK)
                     NCEA = NCE+NDIFF
                     Q1 (NCE) = Q1 (NCE) + .5D0 * QVSH (JA, NCEA, &
                        NLINKA)
496               END DO
               ENDIF

            ENDIF


         ENDIF
!                             SET THE LATERAL FLOW RATES Q1 FOR THE
!                             ENTIRE DEPTH OF FACE JA OF THE
!                             CURRENT COLUMN NCL (incl L-shaped banks)
         DO 511 NCE = NCEBOT, NCETOP
            QQ1 (NCE, JA) = Q1 (NCE) * (ZONE1 * ROH (NCE) / KSP (NCE) )
            QQ (NCE, JA) = QQO (NCL, NCE, JA)
            QQO (NCL, NCE, JA) = QQ1 (NCE, JA)
511      END DO
!                             SET THE OLD AND NEW LATERAL FLOW RATES
!                             FOR THE SATURATED SECTIONS OF THE FACES
!                             OF THE CURRENT COLUMN




605   END DO
!                             ++++++++++++ END OF MAIN LOOP ++++++++++++
!__________________________*
      DO 712 JDUM = 1, 2
         QQQSW (JDUM) = QQQSWO (NCL, JDUM)
         QQQSW (JDUM + 2) = QQQSWO (NCL, JDUM + 2)
         QQQSW1 (JDUM) = - QOC (NCL, JDUM)
         QQQSWO (NCL, JDUM) = QQQSW1 (JDUM)
         QQQSW1 (JDUM + 2) = QOC (NCL, JDUM + 2)
         QQQSWO (NCL, JDUM + 2) = QQQSW1 (JDUM + 2)



712   END DO
!                             SET RATE OF LATERAL SURFACE WATER FLOW
!                             INTO THE FOUR FACES OF THE COLUMN
! Boundary Conditions *
!_____________________*
      NCWELL = NVSWLT (NCL)
      IF (NCWELL.NE.0) THEN
         QQQWEL = - RSZWLO (NCWELL) * AREA (NCWELL)
         QQQWL1 = - QVSWEL (NCWELL) * AREA (NCWELL)
      ELSE
         QQQWEL = zero
         QQQWL1 = zero

      ENDIF
!                             irrigation onto grids
      QI = QIO (NCL)
      QI1 = - PNETTO (NCL) * AREA (NCL)

      QIO (NCL) = QI1
!                             SET RATE OF RAIN WATER INFLOW (NEGATIVE
!                             TO CONFORM TO POSITIVE UPWARDS CONVENTION)
      DO 813 NCE = NAQU, NCETOP
813   WELDRA (NCE) = zero
      IW = NVSWLI (NCL)
      IF (IW.NE.0) THEN
         DO 818 NCE = NWELBT (NCL), NWELTP (NCL)
818      WELDRA (NCE) = QVSWLI (NCE, IW)

      ENDIF
!                             SET THE RATE OF WELL WITHDRAWL FROM
!                             INDIVIDUAL CELLS
      DO 1198 NCE = 1, NCETOP
         QQRV (NCE) = zero
1198  END DO


      IF (ISBK) QQRV (NCEAB (NLINKA, JBK) ) = QBKB (NLINKA, JBK)
!                             SET RATE OF FLOW INTO BANK CELLS FROM
!                             STREAM WATER. FLOW TAKES PLACE ONLY OVER
!                             THE SATURATED DEPTH BETWEEN CELL NCEAB AND
!                             THE EFFECTIVE BED OF THE CHANNEL
!################### temporary code for calc vertical vels. JE 18/9/91
! re-used by GP 24/1/96
! emult: fraction of the error correction which is removed at each cell
      DO 3030 NCE = NCETOP, MAX (1, NCETOP - 4), - 1
3030  EMULT (NCE) = zero
      DO 3032 NCE = NCETOP - 5, MAX (1, NCETOP - 7), - 1
3032  EMULT (NCE) = 0.1D0
      DO 3034 NCE = NCETOP - 8, MAX (1, NCETOP - 19), - 1
3034  EMULT (NCE) = half
      DO 3036 NCE = NCETOP - 20, NCEBOT, - 1

3036  EMULT (NCE) = ONE
      UIN = (DDDSW1 - DDDSW) / (Z2SQOD * TSE)
      DUM = zero
      DO 3120 JA = 1, 4
         DUM = DUM + QQQSW1 (JA)
3120  END DO

      UUAJP1 (NCETOP) = UIN + EEVAP (NCL) + (QI1 - DUM) / AREA (NCL)
      DO 3122 NCE = NCETOP, NCEBOT, - 1
         DUM0 = KSP (NCE) / (ROH (NCE) * ZONE1)
         DUM = KSP (NCE) * (TTHET1 (NCE) - TTHET (NCE) ) / (ROH (NCE) &
            * Z2OD * TSE)
         DUM = DUM + WELDRA (NCE) + TRAN1 (NCE)
         DUM1 = QQRV (NCE) + DUM0 * (QQ1 (NCE, 1) + QQ1 (NCE, 2) &
            + QQ1 (NCE, 3) + QQ1 (NCE, 4) )
         UUOLD = UUAJP1 (NCE-1)
         UUNEW = (DUM - DUM1 / AREA (NCL) + VELDUM (NCE) * UUAJP1 (NCE) &
            ) / VELDUM (NCE-1)
         ERRDUM = UUNEW - UUOLD
         UUAJP1 (NCE-1) = UUNEW - ERRDUM * EMULT (NCE)
         UUAJPO (NCL, NCE-1) = UUAJP1 (NCE-1)

3122  END DO
!################### end of temporary code
      QQRF = QQRFO (NCL)
      QQRF1 = AREA (NCL) * UUAJP1 (NCEBOT - 1)

      QQRFO (NCL) = QQRF1
!                             set rate of flow through base of column





   END SUBROUTINE COLMW




!> Returns the effective longitudinal dispersion coefficient for soil water.
!>
!> The current implementation returns the placeholder constant `3.0D-8`.
!> Although `CMRD` reads manual fields `CM59` and `CM61` into local molecular
!> diffusion and dispersivity tables, those values are not yet used here.
   DOUBLEPRECISION FUNCTION DISP (NCONT, JSOIL, THETA, UM, UP)
!                             (VERTICAL) EFFECTIVE LONGITUDINAL
!                             DISPERSION COEFFICIENT FOR SOIL
      INTEGER, INTENT(IN) :: NCONT !! Contaminant index.
      INTEGER, INTENT(IN) :: JSOIL !! Soil data-set index.
      DOUBLEPRECISION, INTENT(IN) :: THETA !! Soil water content.
      DOUBLEPRECISION, INTENT(IN) :: UM    !! Lower-interface velocity argument.
      DOUBLEPRECISION, INTENT(IN) :: UP    !! Upper-interface velocity argument.
      DISP = 3.0D-8
!                             ########## SOIL INFO NEEDED HERE #########





   END FUNCTION DISP



!> Updates all contaminant concentrations for one channel link.
!>
!> The routine gathers boundary/link concentrations, sediment fractions,
!> bed/loose/suspended sediment state, bank exchange, plant uptake, and
!> retardation factors for each contaminant, then calls [[link]] to solve the
!> link water, bed, and sediment concentration equations.
!>
!> `LINKSM` prepares the three link compartments used by [[link]]: the deeper
!> bed/deposited-material compartment `CCPBD`, the bed-surface compartment
!> `CCPBS`, and the stream-water compartment `CCPSF`. If the link is dry
!> (`USCP <= 0.5`) the stream-water concentration is reset to the incoming
!> rainfall/boundary concentration `CCAPIN`; otherwise it is taken from the old
!> link-water concentration.
!>
!> | Link state | Initial `CCPSF` | Bed-surface infiltration terms |
!> |:-----------|:----------------|:-------------------------------|
!> | `USCP > 0.5` | Previous stream-water concentration `CCCCO(NLINK,NCETOP,NCONT)`. | Calculated with `FRET(CCPSF,...)`. |
!> | `USCP <= 0.5` | Incoming concentration `CCAPIN(NCONT)`. | Set to zero. |
!>
!> At each link end, connected links use the current stream concentration and
!> retardation of every nonzero `LWORK` entry. Boundary, spring, or headwater
!> ends use `CCAPE(NLINK,NCONT)` and unit retardation in the first slot for
!> that end; the remaining two slots are zero.
!>
!> The manual's channel contaminant inputs supply the principal controls used
!> here: Freundlich power `GNN` (`CM43`), decay constant `GGLMSO` (`CM45`),
!> exchange between bed layers `ALPHBD` (`CM47`), exchange between stream water
!> and bed surface `ALPHBS` (`CM49`), and particle-size reference distribution
!> coefficients `KDDLS` (`CM51`). These are combined with current sediment
!> fractions, link geometry, bank concentrations, upstream/downstream boundary
!> concentrations, and wet/dry contaminant inputs.
!>
!> Deposited and suspended particle-size fractions are normalised from the
!> current sediment masses before retardation factors are calculated:
!>
!> \[
!> \beta^{d}_j =
!> \begin{cases}
!> GINFD_j / \sum_k GINFD_k, & \sum_k GINFD_k > 0,\\
!> 0, & \sum_k GINFD_k = 0,
!> \end{cases}
!> \qquad
!> \beta^{s}_j =
!> \begin{cases}
!> GINFS_j / \sum_k GINFS_k, & \sum_k GINFS_k > 0,\\
!> 0, & \sum_k GINFS_k = 0.
!> \end{cases}
!> \]
!>
!> The effective new bed sediment composition mixes loose bed material and
!> newly exposed original bed material:
!>
!> \[
!> SSBED^{n+1}_j =
!> \frac{DLS\,CWIDTH\,FBETA_j
!>       + (ACPBD1-ACPBS)\,Z2SQ\,SOSDFN_j}
!>      {DLS\,CWIDTH + (ACPBD1-ACPBS)\,Z2SQ}.
!> \]
!>
!> `FRET` is then called for the bed, bed-surface, stream-water, and deposited
!> sediment compartments to obtain retardation factors and their concentration
!> and time derivatives. The stream input/source terms passed to [[link]] are
!>
!> \[
!> ICP1 = -IIICF\,AREA /(D0\,CLENTH),
!> \]
!>
!> for wet/dry contaminant loading, and
!>
!> \[
!> QCP1 =
!> \frac{(QQQSL1-QQQDUM)\,CCAPI + QQQDUM\,C_{well}}
!>      {D0\,Z2\,KS},
!> \]
!>
!> with the well term omitted when no irrigation well is active. The exchange
!> coefficients are scaled to the link cross-section as
!>
!> \[
!> ACSBD1 = CWIDTH\,ALPHBD/D0,\qquad
!> ACSBS1 = CWIDTH\,ALPHBS/D0.
!> \]
!>
!> After [[link]] returns updated concentrations, `LINKSM` writes the new
!> bed, bed-surface, and stream-water concentrations back to `CCCC`. It also
!> stores the updated concentration and retardation of the current contaminant
!> in `CCBD1Q`, `CCBS1Q`, `CCSF1Q`, `FCBD1Q`, `FCBS1Q`, and `FCSF1Q` so the
!> next contaminant in a decay/generation chain can use them as parent terms:
!>
!> \[
!> F^{n+1} = F^n + F_t\,\Delta t + F_c\,(C^{n+1}-C^n).
!> \]
   SUBROUTINE LINKSM (NLINK)

      USE CONT_CC
      USE SED_CS
      USE CONT_CC
      USE COLM_C1
      USE LINK_CC
      USE LINK_CW
      USE SED_CO
      USE PLANT_CC

!                             INCLUDE ALL THE PARAMETER STATEMENTS
!                             AND COMMON BLOCKS NEEDED
!COMMON / LK1 / ISLK (2), LWORK (6), NBK (2), qqqdum, QQQSL1
!common / temp / nwell
!##### nwell and qqqdum used in temporary irrigation code###########
!LOGICAL :: ISLK
!                             VARIABLES USED ONLY IN LINKW AND LINKSM

      INTEGER, INTENT(IN) :: nlink !! Channel-link element being updated.
      INTEGER :: ncont, nce, jlend, jdum, jla, jsed, na, lfone, ldum, la
      DOUBLEPRECISION FBTAD (NSEDEE), FBTAS (NSEDEE), KDDUM (NSEDEE), &
         SSBED1 (NSEDEE), SSBED (NSEDEE), SSD1 (NSEDEE), SSD (NSEDEE), &
         SSF1 (NSEDEE), SSF (NSEDEE)
      DOUBLEPRECISION :: ccpbd, ccpbs, qcdum, sumd, sums, dddsum, pb, fdum, fdumc, &
         fdumt, dum, arl, arp, ccpsf, dddum, dsdum, ccpbd1, ccpbs1, ccpsf1, &
         dumx
      CCBD1Q = zero
      CCBS1Q = zero
      CCSF1Q = zero
      FCBD1Q = zero
      FCBS1Q = zero
      FCSF1Q = zero


      GCPLAQ = zero
!                             SET PARENT CONCENTRATIONS AND RETARDATION
!                             VARIABLES TO O FOR 1ST PASS OF DO LOOP 100
      DO 100 NCONT = 1, NCON
         DO 102 JBK = 1, 2
            CCPBK (JBK, 1) = CCCCO (NBK (JBK), 1, NCONT)
!                             THIS ELEMENT OF ARRAY CCCCO IS USED TO
!                             HOLD THE EFFECTIVE CONCENTRATION IN
!                             THE FLOW ENTERING THE STREAM VIA THE
!                             STREAM BED
            DO 104 NCE = NCEBK (JBK), NCETOP
               CCPBK (JBK, NCE) = CCCCO (NBK (JBK), NCE, NCONT)
               SCPBK (JBK, NCE) = SSSSO (NBK (JBK), NCE, NCONT)
104         END DO
            CCPGS1 (JBK) = CCCC (NBK (JBK), NCETOP, NCONT)
102      END DO
         CCPBD = CCCCO (NLINK, NCETOP - 2, NCONT)
         CCPBS = CCCCO (NLINK, NCETOP - 1, NCONT)
         IF (USCP.GT.half) THEN
            CCPSF = CCCCO (NLINK, NCETOP, NCONT)
         ELSE
            CCPSF = ccapin (ncont)
!                             IF THERE IS NO WATER IN LINK


         ENDIF
         DO 110 JLEND = 1, 2
            IF (ISLK (JLEND) ) THEN
!                             THERE ARE OTHER LINKS ASSOCIATED WITH END
!                             JLEND OF THE CURRENT LINK
               DO 112 JDUM = 1, 3
                  JLA = (JLEND-1) * 3 + JDUM
                  LA = LWORK (JLA)
                  IF (LA.NE.0) THEN
                     CCSFA1 (JLA) = CCCC (LA, NCETOP, NCONT)
                     FCSFA1 (JLA) = FSF (LA, NCONT) + FSFT (LA, NCONT) &
                        * TSE+FSFC (LA, NCONT) * (CCSFA1 (JLA) - CCCCO (LA, &
                        NCETOP, NCONT) )
                  ELSE
                     CCSFA1 (JLA) = zero
                     FCSFA1 (JLA) = zero
                  ENDIF

112            END DO
            ELSE
!                             END JLEND OF LINK IS AT CATCHMENT BOUNDARY
!                             THE HEAD OF A STREAM, OR A SPRING
               JLA = (JLEND-1) * 3 + 1
               CCSFA1 (JLA) = CCAPE (NLINK, NCONT)
               FCSFA1 (JLA) = one
!                             FOR FLOW INTO CATCHMENT OR SPRING
               DO 114 JDUM = 2, 3
                  JLA = (JLEND-1) * 3 + JDUM
                  CCSFA1 (JLA) = zero
                  FCSFA1 (JLA) = zero
114            END DO
            ENDIF



110      END DO
!                             SET LINK AND BANK CONCENTRATIONS.
!                             NB: IF THE STREAM IS DRY, THE STREAM WATER
!                             CONCENTRATION SET TO THE CONCENTRATION IN
!                             RAIN WATER
         ICP1 = - IIICF (NCONT) * AREA (NLINK) / (D0 * CLENTH (NLINK) )
!#######################################################################
         qcdum = (qqqsl1 - qqqdum) * ccapi (ncont)
         if (nwell.ne.0) qcdum = qcdum + qqqdum * ccccw (nwell, ncont)


         QCP1 = qcdum / (D0 * Z2 * KS)
!       QCP1 = QQQSL1*CCAPI(NCONT)/(D0*Z2*KS)
!                             SET VARIABLES FOR WET AND DRY INPUT OF
!                             CONTAMINANT FROM ABOVE
!######## temporary code for inclusion of irrigation water in rain water
         SUMD = zero
         SUMS = zero
         DO 150 JSED = 1, NSED
            SUMD = SUMD+GINFD (NLINK, JSED)
            SUMS = SUMS + GINFS (NLINK, JSED)
            KDDUM (JSED) = KDDLS (JSED, NCONT)
150      END DO
         IF (ISZERO(SUMD)) then
            dddum = one
         else
            dddum = sumd
         endif
         IF (ISZERO(SUMS)) then
            dsdum = one
         else
            dsdum = sums
         endif
         DO 152 JSED = 1, NSED
            FBTAD (JSED) = GINFD (NLINK, JSED) / dddum
            FBTAS (JSED) = GINFS (NLINK, JSED) / dsdum

152      END DO
!                             SCALE RATES OF INFLITRATION TO GIVE THE
!                             FRACTIONS IN EACH GROUP OF AN EFFECTIVE
!                             SOIL. THE EFFECTIVE SOIL IS THAT WHICH IF
!                             ERODED AT A RATE EQUAL TO THE TOTAL RATE
!                             OF INFILTRATION WOULD RELEASE THE CORRECT
!                             AMOUNT OF SEDIMENTS FOR INFILTRATION

         PB = PBSED (NLINK)
         FDUM = zero
         FDUMC = zero
         FDUMT = zero
         CALL FRET (CCPBS, GNN (NCONT), PB, PB, FBTAD, FBTAD, KDDUM, PB, &
            PB, PB, FDUM, FDUMC, FDUMT, TSE, NSED, ISADNL)
         DUM = SUMD * CCPBS / CLENTH (NLINK)
         ICPSBD = (FDUM - PB) * DUM
         ICSBDC = FDUMC * DUM + ICPSBD
         ICSBDT = FDUMT * DUM
!                             SET INFILTRATION VARIABLES FOR BED DEEP
!                             LAYER
         IF (USCP.LT.half) THEN
!                             THERE IS NO WATER IN LINK
            ICPSBS = zero
            ICSBSC = zero
            ICSBST = zero
         ELSE
            CALL FRET (CCPSF, GNN (NCONT), one, one, FBTAS, FBTAS, &
               KDDUM, zero, zero, zero, FDUM, FDUMC, FDUMT, TSE, NSED, &
               ISADNL)
            DUM = SUMD * CCPSF / CLENTH (NLINK)
            ICPSBS = (FDUM - PB) * DUM
            ICSBSC = FDUMC * DUM + ICPSBS
            ICSBST = FDUMT * DUM


         ENDIF
!                             SET INFILTRATION VARIABLES FOR BED SURFACE
!                             LAYER
         ARL = DLS (NLINK) * CWIDTH (NLINK)
!                             X-SECIONAL AREA OF LOOSE SEDIMENTS IN BED
         ARP = (ACPBD1 - ACPBS) * Z2SQ
!                             X-SECTIONAL AREA OF NON-ERODED PARENT
!                             MATERIAL WITHIN BED DEEP LAYER
         DUM = one / (ARL + ARP)
         DO 200 JSED = 1, NSED
            SSBED1 (JSED) = DUM * (ARL * FBETA (NLINK, JSED) + ARP * &
               SOSDFN (NSOBED (NLINK), JSED) )
            SSBED (JSED) = FBBEDO (NLINK, JSED)
            FBBEDO (NLINK, JSED) = SSBED1 (JSED)
            SSF1 (JSED) = FDEL (NLINK, JSED)
            SSF (JSED) = FDELO (NLINK, JSED)
            FDELO (NLINK, JSED) = SSF1 (JSED)
            SSD1 (JSED) = FBTSD (NLINK, JSED)
            SSD (JSED) = FBTSDO (NLINK, JSED)
            FBTSDO (NLINK, JSED) = SSD1 (JSED)

200      END DO

         CALL FRET (CCPBD, GNN (NCONT), THBEDO (NLINK), THBED (NLINK), &
            SSBED, SSBED1, KDDUM, PB, PB, PB, FCPBD, FCPBDC, FCPBDT, TSE, &
            NSED, ISADNL)

         CALL FRET (CCPBS, GNN (NCONT), THBEDO (NLINK), THBED (NLINK), &
            SSBED, SSBED1, KDDUM, PB, PB, PB, FCPBS, FCPBSC, FCPBST, TSE, &
            NSED, ISADNL)
         CALL FRET (CCPSF, GNN (NCONT), one, one, SSF, SSF1, KDDUM, &
            zero, zero, zero, FCPSF, FCPSFC, FCPSFT, TSE, NSED, ISADNL)
         fsf (nlink, ncont) = fcpsf
         fsfc (nlink, ncont) = fcpsfc

         fsft (nlink, ncont) = fcpsft
!                                       save retardation factors for con


         CALL FRET (CCPSF, GNN (NCONT), one, one, SSD, SSD1, KDDUM, &
            zero, zero, zero, FCPSD, FCPSDC, FCPSDT, TSE, NSED, ISADNL)
!                             SET REATRDATION VARIABLES FOR THE BED DEEP
!                             LAYER, BED SURFACE LAYER, STREAM WATER,
!                             AND NEWLY DEPOSITED SEDIMENTS
         DO 250 JBK = 1, 2
            NA = NBK (JBK)
            FCPSW1 (JBK) = RSW (NA, NCONT) + RSWT (NA, NCONT) * TSE+ &
               RSWC (NA, NCONT) * (CCCC (NA, NCETOP, NCONT) - CCPBK (JBK, &
               NCONT) )
            DO 252 NCE = NCEBK (JBK), NCETOP
               FCPBK (JBK, NCE) = FCPBKO (NLINK, JBK, NCE, NCONT)
               GCPBK (JBK, NCE) = GCPBKO (NLINK, JBK, NCE, NCONT)
252         END DO
!                             NB: FCPBKO AND GCPBKO CALCULATED IN COLMSM


250      END DO
!                             SET RETRDATION VARIABLES FOR THE DYNAMIC
!                             AND DEAD SPACE REGIONS OF THE ERODING
!                             BANK SOIL
         ECPBD = zero
         ECPBDC = zero
         ECPBDT = zero
         ECPBS = zero
         ECPBSC = zero
         ECPBST = zero
         ECPSF = zero
         ECPSFC = zero

         ECPSFT = zero
!                             SET RATES OF PLANT UPTAKE
         DUM = CWIDTH (NLINK) / D0
         ACSBD1 = DUM * ALPHBD (NCONT)
         ACSBS1 = DUM * ALPHBS (NCONT)

         GCPLAL = GCPLA (NCONT)
!                             SET CONTAMINANT INFILTRATION RATE WITH
!                             SEDIMENT; AND CONTAMINANT DECAY RATE
         CCPBD1 = zero
         CCPBS1 = zero
         CCPSF1 = zero


         CALL LINK (CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE, &
            NCETOP)
!                             CALCULATES AND RETURNS UPDATED
!                             CONCENTRATIONS
         CCCC (NLINK, NCETOP - 2, NCONT) = CCPBD1
         CCCC (NLINK, NCETOP - 1, NCONT) = CCPBS1


         CCCC (NLINK, NCETOP, NCONT) = CCPSF1
!                             SAVE UPDATED CONCENTRATIONS IN THE GLOBAL
!                             ARRAYS
         CCBD1Q = CCPBD1
         CCBS1Q = CCPBS1
         CCSF1Q = CCPSF1
         FCBD1Q = FCPBD+FCPBDT * TSE+FCPBDC * (CCPBD1 - CCPBD)
         FCBS1Q = FCPBS + FCPBST * TSE+FCPBSC * (CCPBS1 - CCPBS)
         FCSF1Q = FCPSF + FCPSFT * TSE+FCPSFC * (CCPSF1 - CCPSF)


         GCPLAQ = GCPLAL
!                             SET CONCENTRATIONS, RETARDATION, AND DECAY
!                             VARIABLES FOR PARENT CONTAMINANT FOR NEXT
!                             PASS OF DO LOOP 100

100   END DO
      RETURN
   END SUBROUTINE LINKSM



!> Prepares water-flow and geometry terms for a contaminant link solve.
!>
!> `LINKW` determines link orientation, connected links, boundary/adjacent-link
!> flows, surface-water storage, bed storage, bank exchange, sediment-driven
!> bed changes, and scaled Peclet/source terms used by [[linksm]] and [[link]].
!>
!> The routine first maps the physical link orientation onto the local
!> upstream/downstream end and face arrays. `LENDA` identifies which face of
!> each adjacent link is connected to the current link, `ISLK` records whether
!> each link end is connected to other links or to a boundary/spring/headwater,
!> and `LWORK(1:6)` stores up to three connected links at each end.
!>
!> | `LINKNS` | `LFONE` | `LENDA(1:6)` |
!> |:---------|:--------|:-------------|
!> | `.TRUE.` | 2 | `2, 2, 1, 1, 1, 2` |
!> | `.FALSE.` | 1 | `1, 2, 2, 2, 1, 1` |
!>
!> Link-end connectivity is interpreted as:
!>
!> | Topology reference | Meaning | Local storage |
!> |:-------------------|:--------|:--------------|
!> | Positive `ICMREF` reference | One linked neighbour. | Store in a `LWORK` slot chosen from the reciprocal face code. |
!> | Negative `ICMREF` reference | Multi-link junction. | Fill the three slots from `ICMRF2(-ref,3:1)`. |
!> | Zero reference | Boundary, spring, or headwater. | `ISLK(end)=.FALSE.` and all three slots are zero. |
!>
!> Link storage terms are converted to the scaled quantities used by [[link]]:
!>
!> \[
!> ACPBD1 = ACPBI + ARBDEP/Z2SQ,\qquad
!> ACPBS = ACPBSG,\qquad
!> ACPSF1 = ARXL/Z2SQ.
!> \]
!>
!> If `ACPSF1` is effectively zero the link is treated as dry (`USCP = 0`) and
!> the surface-water and bed-storage time derivatives are set to zero. For wet
!> links,
!>
!> \[
!> ACPBDT = (ACPBD1-ACPBDO)/TSE,\qquad
!> ACPSFT = (ACPSF1-ACPSFO)/TSE,
!> \]
!>
!> and the scaled bed-storage change is
!>
!> \[
!> WCPBD1 = Z2SQOD\,ACPBDT/ACPBD1.
!> \]
!>
!> Dry links also have both bank inflow terms `QBKB(NLINK,1:2)` forced to zero
!> before the contaminant equations are assembled.
!>
!> Adjacent-link and boundary flow terms are normalised by the active
!> surface-water storage using
!>
!> \[
!> PCSFA1_a =
!> \frac{-QLINK_a - QDEFF_a\,C_{sed}}{D0\,Z2\,ACSFA1_a},
!> \]
!>
!> where the current implementation sets the dispersed-sediment concentration
!> contribution `C_{sed}` to zero. Boundary/headwater/spring inflow uses the
!> same scaling with the current link flow:
!>
!> \[
!> PCSFA1 = QLINK /(D0\,Z2\,ACSFA1).
!> \]
!>
!> For the current link, the two end-flow terms passed to [[link]] are
!>
!> \[
!> PCSFM1 = \frac{QLINK_1}{D0\,Z2\,ACPSF1},\qquad
!> PCSFP1 = \frac{QLINK_2}{D0\,Z2\,ACPSF1},
!> \]
!>
!> with both set to zero for dry links. The rainfall/effective input to the
!> stream is stored as
!>
!> \[
!> QQQSL1 = -PNETTO\,AREA,
!> \]
!>
!> using the contaminant convention that upward flow is positive; `QQQDUM`
!> similarly stores irrigation-well input when one is connected.
!>
!> Bank and stream-bank exchange terms are scaled by the half-link length
!> \(KS = CLENTH/Z2\). For bank `b` and exposed bank cell `k`,
!>
!> \[
!> PCPBK1_{b,k} =
!> -\frac{QVSH_{b,k}}{D0\,Z2\,KS},\qquad
!> PCPSB1_b =
!> -\frac{QBKB_b}{D0\,Z2\,KS},
!> \]
!>
!> while overland/channel face exchange is signed by bank side:
!>
!> \[
!> PCPSW1_b =
!> s_b\,\frac{QOC_b}{D0\,Z2\,KS},\qquad s_b = 2b-3.
!> \]
!>
!> Finally, `LINKW` updates the stream-bed moisture content used in link
!> retardation. It forms a thickness-weighted average over the two adjacent
!> bank soil columns between the bed-deep and bed-surface limits, including
!> fractional bottom/top cells from `FNCEBD` and `FHBED`, and caps the result at
!> the bed porosity:
!>
!> \[
!> THBED = \min\left(PBSED,\,
!> \frac{\sum_k VSTHE_k\,w_k}{\sum_k w_k}\right),
!> \qquad w_k = DELTAZ_k/Z2 \text{ with fractional end-cell weights.}
!> \]
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1997-02-18 | RAH | 4.1 | Swapped `QVSH`, `DELTAZ`, and `VSTHE` subscripts. |
!> @endhistory
   SUBROUTINE LINKW (NLINK)

      USE SED_CS
      USE COLM_C1
      USE COLM_CG

      USE COLM_CO
!####################temporary, for irrigation
      USE LINK_CC
      USE LINK_CW
      USE BK_CW

      USE PLANT_CC
!                             INCLUDE ALL THE PARAMETER STATEMENTS
!                             AND COMMON BLOCKS NEEDED
!COMMON / LK1 / ISLK (2), LWORK (6), NBK (2), qqqdum, QQQSL1
!common / temp / nwell
!##### nwell and qqqdum used in temporary irrigation code###########
!LOGICAL :: ISLK
      INTEGER, INTENT(IN) :: nlink !! Channel-link element whose water-flow terms are prepared.
      INTEGER :: jlend, jdum, lfone, ldum, jla, jfdum, jfdumb, nce, jvegbk, &
         ndum, la
      DOUBLEPRECISION :: dumx, dum, duma, dmult, sumk, sum, dumk
!                             VARIABLES USED ONLY IN LINKW AND LINKSM
      IF (LINKNS (NLINK) ) THEN
         LENDA (1) = 2
         LENDA (2) = 2
         LENDA (3) = 1
         LENDA (4) = 1
         LENDA (5) = 1
         LENDA (6) = 2
      ELSE
         LENDA (1) = 1
         LENDA (2) = 2
         LENDA (3) = 2
         LENDA (4) = 2
         LENDA (5) = 1
         LENDA (6) = 1

      ENDIF
!                       SET POINTERS FOR THE END OF THE LINKS WHICH
!                       CAN BE ATTACHED TO A GIVEN LINK
      ACPBD1 = ACPBI (NLINK) + ARBDEP (NLINK) / Z2SQ
      ACPBS = ACPBSG (NLINK)
      ACPSF1 = ARXL (NLINK) / Z2SQ
      IF (ACPSF1.LT.1.0D-20) THEN
         USCP = zero
         ACPBDT = zero
         ACPSFT = zero
         QBKB (NLINK, 1) = zero
         QBKB (NLINK, 2) = zero
!                             ENSURES BED LAYER CALCULATIONS ARE
!                             CORRECT IF THERE IS NO WATER IN LINK
      ELSE
         USCP = one
         ACPBDT = (ACPBD1 - ACPBDO (NLINK) ) / TSE
         ACPSFT = (ACPSF1 - ACPSFO (NLINK) ) / TSE
      ENDIF
      ACPBDO (NLINK) = ACPBD1
      ACPSFO (NLINK) = ACPSF1
      WCPBD1 = Z2SQOD * ACPBDT / ACPBD1

      VCPBK1 = Z2OD * GNUBK (NLINK)
!                             SET SCALED VARIABLES FOR AREA AND EROSION
      NBK (1) = NBANK (NLINK, 1)
      NBK (2) = NBANK (NLINK, 2)
      NCEBK (1) = NHBED (NLINK, 1) + 1


      NCEBK (2) = NHBED (NLINK, 2) + 1
!                             SET LOCAL BANK NUMBERS AND NUMBERS FOR THE
!                             BOTTOM CELLS FOR THE EXPOSED PART OF THE
!                             BANK SOIL COLUMNS
      IF (LINKNS (NLINK) ) THEN
         LFONE = 2
      ELSE
         LFONE = 1

      ENDIF
!                             SET NUMBER FOR THE FACE OF THE LINK WHICH
!                             IS AT END ONE OF THE LINK
      LDUM = ICMREF (NLINK, LFONE+4)
      IF (LDUM.GT.0) THEN
!                             THERE IS ONLY ONE OTHER LINK ASSOCIATED
!                             WITH END ONE OF THE CURRENT LINK
         ISLK (1) = .TRUE.
         LWORK (1) = 0
         LWORK (2) = 0
         LWORK (3) = 0
         if (linkns (nlink) ) then
            if (icmref (nlink, 10) .eq.3) lwork (1) = ldum
            if (icmref (nlink, 10) .eq.4) lwork (2) = ldum
            if (icmref (nlink, 10) .eq.1) lwork (3) = ldum
         else
            if (icmref (nlink, 9) .eq.2) lwork (1) = ldum
            if (icmref (nlink, 9) .eq.3) lwork (2) = ldum
            if (icmref (nlink, 9) .eq.4) lwork (3) = ldum
         endif
!                             LWORK HOLDS THE NUMBERS OF THE LINKS
!                             ASSOCIATED WITH THE CURRENT LINK
      ELSEIF (LDUM.LT.0) THEN
!                             THERE IS MORE THAN ONE LINK ASSOCIATED
!                             WITH END ONE OF THE CURRENT LINK
         ISLK (1) = .TRUE.
         LWORK (1) = ICMRF2 ( - LDUM, 3)
         LWORK (2) = ICMRF2 ( - LDUM, 2)
         LWORK (3) = ICMRF2 ( - LDUM, 1)
      ELSE
!                             THERE IS NO LINKS ASSOCIATED WITH END ONE
!                             OF THE CURRENT LINK
         ISLK (1) = .FALSE.
         LWORK (1) = 0
         LWORK (2) = 0
         LWORK (3) = 0


      ENDIF
      LDUM = ICMREF (NLINK, LFONE+6)
      IF (LDUM.GT.0) THEN
!                             THERE IS ONLY ONE OTHER LINK ASSOCIATED
!                             WITH END TWONE OF THE CURRENT LINK
         ISLK (2) = .TRUE.
         LWORK (4) = 0
         LWORK (5) = 0
         LWORK (6) = 0
         if (linkns (nlink) ) then
            if (icmref (nlink, 12) .eq.1) lwork (4) = ldum
            if (icmref (nlink, 12) .eq.2) lwork (5) = ldum
            if (icmref (nlink, 12) .eq.3) lwork (6) = ldum
         else
            if (icmref (nlink, 11) .eq.4) lwork (4) = ldum
            if (icmref (nlink, 11) .eq.1) lwork (5) = ldum
            if (icmref (nlink, 11) .eq.2) lwork (6) = ldum
         endif
!                             LWORK HOLDS THE NUMBERS OF THE LINKS
!                             ASSOCIATED WITH THE CURRENT LINK
      ELSEIF (LDUM.LT.0) THEN
!                             THERE IS MORE THAN ONE LINK ASSOCIATED
!                             WITH END TWO OF THE CURRENT LINK
         ISLK (2) = .TRUE.
         LWORK (4) = ICMRF2 ( - LDUM, 3)
         LWORK (5) = ICMRF2 ( - LDUM, 2)
         LWORK (6) = ICMRF2 ( - LDUM, 1)
      ELSE
!                             THERE IS NO LINKS ASSOCIATED WITH END TWO
!                             OF THE CURRENT LINK
         ISLK (2) = .FALSE.
         LWORK (4) = 0
         LWORK (5) = 0
         LWORK (6) = 0

      ENDIF
!                             SET LWORK, THE ARRAY HOLDING THE NUMBERS
!                             THE LINKS ASSOCIATED WITH THE CURRENT LINK
      DUMX = one / (D0 * Z2)
      DO 100 JLEND = 1, 2
         IF (ISLK (JLEND) ) THEN
!                             THERE ARE OTHER LINKS ASSOCIATED WITH END
!                             JLEND OF THE CURRENT LINK
            DO 102 JDUM = 1, 3
               JLA = (JLEND-1) * 3 + JDUM
               LA = LWORK (JLA)
               IF (LA.NE.0) THEN
                  ACSFA1 (JLA) = MAX (1.0d-6, ACPSFO (LA) )
                  DUM = zero
                  PCSFA1 (JLA) = DUMX * ( - QLINK (LA, LENDA (JLA) ) &
                     - QDEFF (LA, LENDA (JLA) ) * DUM) / ACSFA1 (JLA)
!                             NB: CONVECTION WITH DISPERSED SEDIMENTS
!                             NEGLECTED
               ELSE
                  ACSFA1 (JLA) = zero
                  PCSFA1 (JLA) = zero
               ENDIF

102         END DO
         ELSE
!                             END JLEND OF LINK IS AT CATCHMENT BOUNDARY
!                             THE HEAD OF A STREAM, OR A SPRING
            JLA = (JLEND-1) * 3 + 1
            ACSFA1 (JLA) = MAX (1.0d-6, ACPSFO (NLINK) )
            PCSFA1 (JLA) = DUMX * QLINK (NLINK, JLEND) / ACSFA1 (JLA)
!                             FOR FLOW INTO CATCHMENT OR FROM SPRING
            DO 104 JDUM = 2, 3
               JLa = (JLEND-1) * 3 + JDUM
               ACSFA1 (JLA) = zero
               PCSFA1 (JLA) = zero
104         END DO
         ENDIF
100   END DO
      IF (USCP.LT.half) THEN
         PCSFM1 = zero
         PCSFP1 = zero
      ELSE
         DUM = DUMX / ACPSF1
         DUMA = zero
         PCSFM1 = DUM * (QLINK (NLINK, 1) + DUMA * QDEFF (NLINK, 1) )
         PCSFP1 = DUM * (QLINK (NLINK, 2) + DUMA * QDEFF (NLINK, 2) )


      ENDIF
!                             SET AREA AND PECLET NUMBER FOR THE LINKS
!                             ASSOCIATED WITH THE CURRENT LINKS
      QQQSL1 = - PNETTO (NLINK) * AREA (NLINK)
!                             -VE RATE OF RAIN ARRIVAL AT LINK
!                             (+VE UPWARDS TO CONFORM TO CONVENTION)
!#######################################################################
      nwell = NVSWLT (nlink)
      if (nwell.ne.0) then
         qqqdum = - rszwlo (nwell) * area (nwell)
      else
         qqqdum = zero


      endif
!###########temporary, qqqdum is rate of input of well water to stream##

      KS = CLENTH (NLINK) / Z2
!                             SET SCALED LENGTH OF LINK
      DUM = DUMX / KS
      DO 150 JBK = 1, 2
         JFDUM = 2 * JBK - LFONE+1
!                             FACE NUMBER FOR LINK, ACROSS WHICH WATER
!                             ENTERS FROM BANK JBK

         JFDUMB = ICMREF (NLINK, JFDUM + 8)
!                               FACE NUMBER FOR BANK, POINTING TOWARDS L
         DO 152 NCE = NCEBK (JBK), NCETOP
            PCPBK1 (JBK, NCE) = - DUM * QVSH (JFDUMB, NCE, NBK (JBK) )
152      END DO
         PCPSB1 (JBK) = - DUM * QBKB (NLINK, JBK)
         DMULT = DBLE (2 * JBK - 3)
!                             MULTIPLIER USED TO OBTAIN CORRECT SIGN FOR
!                             FLOWS INTO THE LINK
         PCPSW1 (JBK) = DMULT * DUM * QOC (NLINK, JFDUM)
         JVEGBK = NVC (NBK (JBK) )
         NDUM = NCEBD (NLINK, JBK) + 1
!                             k IS ONLY USED FOR THE CELLS AT OR ABOVE
!                             THE LEVEL OF THE BOTTOM OF THE BED DEEP
!                             LAYER
         DO 154 NCE = NDUM, NCETOP
            KSPBK (JBK, NCE) = DELTAZ (NCE, NBK (JBK) ) / z2
154      END DO



150   END DO
!                             SET VALUES OF VARIABLES ASSOCIATED WITH
!                             THE ADJACENT STREAM BANKS
      SUMK = zero
      SUM = zero
      DO 160 JBK = 1, 2
         NCE = NDUM
         DUMK = (one - FNCEBD (NLINK, JBK) ) * KSPBK (JBK, NCE)
         SUMK = SUMK + DUMK
         SUM = SUM + VSTHE (NCE, NBK (JBK) ) * DUMK
         DO 162 NCE = NDUM + 1, NHBED (NLINK, JBK)
            DUMK = KSPBK (JBK, NCE)
            SUMK = SUMK + DUMK
            SUM = SUM + VSTHE (NCE, NBK (JBK) ) * DUMK
162      END DO
         NCE = NHBED (NLINK, JBK) + 1
         DUMK = FHBED (NLINK, JBK) * KSPBK (JBK, NCE)
         SUMK = SUMK + DUMK
         SUM = SUM + VSTHE (NCE, NBK (JBK) ) * DUMK
160   END DO
      THBEDO (NLINK) = THBED (NLINK)



      THBED (NLINK) = MIN(PBSED (NLINK), SUM / SUMK)
!                             SET MOISTURE CONTENT IN STREAM BED, AS THE
!                             WEIGHTED AVERAGE FOR THE CELLS, OF BOTH
!                             BANKS, LYING WITHIN THE BED SURFACE AND
!                             BED DEEP LAYERS
      RETURN



   END SUBROUTINE LINKW
! 15/1/96
!                             4/9/91



!> Solves the coupled contaminant difference equations for one channel link.
!>
!> The routine sets up and solves the stream-link difference equations with
!> fully implicit coupling to the banks. The link equations couple bed/deposited
!> material, bed-surface material, and surface-water concentration compartments.
!> Nonlinear terms are solved with [[snl3]], including special handling for
!> dry/no-water cases.
!>
!> The unknowns are timestep concentration rates for the stream-water,
!> bed-surface, and deeper-bed compartments:
!>
!> \[
!> X_1 = WMESF,\qquad X_2 = WMEBS,\qquad X_3 = WMEBD,
!> \]
!>
!> which are applied after the solve as
!>
!> \[
!> CCPSF^{n+1} = CCPSF^n + TSE\,X_1,\quad
!> CCPBS^{n+1} = CCPBS^n + TSE\,X_2,\quad
!> CCPBD^{n+1} = CCPBD^n + TSE\,X_3.
!> \]
!>
!> The coefficient assembly uses upwind signs from the scaled flow terms
!> prepared by [[linkw]]. For example, the stream end outflow rate is
!>
!> \[
!> DUMA1 = \frac{\max(0,-PCSFP1)+\max(0,-PCSFM1)}{KS},
!> \]
!>
!> bank-column outflow and inflow contributions are
!>
!> \[
!> DUMA5 = \sum_{b,k}\max(0,-PCPBK1_{b,k}),\qquad
!> DUMP6 = \sum_{b,k}\max(0,PCPBK1_{b,k})\,CCPBK_{b,k},
!> \]
!>
!> and bank storage entering the link is accumulated as
!>
!> \[
!> DUMP5 =
!> VCPBK1 \sum_{b,k}
!> \left(FCPBK_{b,k}CCPBK_{b,k}
!>      + GCPBK_{b,k}SCPBK_{b,k}\right)KSPBK_{b,k}.
!> \]
!>
!> The current concentrations and retardation linearisations are folded into
!> coefficient groups `ALT`, `BLT`, `DLT`, `ELT`, `FLT`, `HLT`, and `GYLT`.
!> Their starred forms (`ALTSTR`, `BLTSTR`, etc.) are the nonlinear adsorption
!> derivatives, and the `...DA` terms are the fully implicit exchange terms
!> between stream water, bed surface, and deeper bed. In the wet-link case the
!> assembled system passed to [[snl3]] is
!>
!> \[
!> \begin{aligned}
!> (A + A_s X_1)X_1 - (B + B_s X_2)X_2 &= P,\\
!> -(D + D_s X_1)X_1 + (E + E_s X_2)X_2
!>      - (F + F_s X_3)X_3 &= Q,\\
!> -(H + H_s X_2)X_2 + (Y + Y_s X_3)X_3 &= S,
!> \end{aligned}
!> \]
!>
!> with the code mapping
!>
!> \[
!> \begin{array}{lll}
!> A=ALT-DLTDA, & A_s=ALTSTR, & P=PLT-QLTDA-SLTDA,\\
!> B=-BLT+ELTDA+HLTDA, & B_s=-BLTSTR, & \\
!> D=-DLT-DLTDA, & D_s=-DLTSTR, & Q=QLT+QLTDA,\\
!> E=ELT+ELTDA, & E_s=ELTSTR, & \\
!> F=-FLT-FLTDA, & F_s=-FLTSTR, & \\
!> H=-HLT-HLTDA, & H_s=-HLTSTR, & S=SLT+SLTDA,\\
!> Y=GYLT+GYLTDA, & Y_s=GYLTSR. &
!> \end{array}
!> \]
!>
!> `PLT`, `QLT`, and `SLT` are the right-hand-side residuals for the stream,
!> bed-surface, and deeper-bed balances. They include old storage,
!> parent-contaminant generation, decay, rainfall/well input (`QCP1`, `ICP1`),
!> adjacent-link inflow, bank and ground-surface exchange, erosion/deposition,
!> plant terms, and sediment/adsorption storage terms assembled in [[linksm]].
!>
!> When the link is dry, `LINK` removes the stream-water equation by calling
!> [[snl3]] with \(A=1\), \(P=0\), and all stream coupling in the first equation
!> set to zero. This forces \(WMESF=0\) while still solving the coupled
!> bed-surface and deeper-bed equations.
!>
!> | Link state | `SNL3` treatment | Updated concentration |
!> |:-----------|:-----------------|:----------------------|
!> | Wet (`USCP >= 0.5`) | Solve all three coupled rate equations. | `CCPSF`, `CCPBS`, and `CCPBD` are all advanced by `TSE*rate`. |
!> | Dry (`USCP < 0.5`) | Replace stream-water row with `WMESF=0`. | Bed-surface and deeper-bed compartments still solve. |
   SUBROUTINE LINK (CCPBD, CCPBD1, CCPBS, CCPBS1, CCPSF, CCPSF1, TSE, &
      NCETOP)
      USE LINK_CC

      USE LINK_CC1
      INTEGER, INTENT(IN) :: NCETOP !! Top active contaminant cell in the link column.
      DOUBLEPRECISION, INTENT(IN) :: CCPBD  !! Old deeper-bed/deposited-material concentration.
      DOUBLEPRECISION, INTENT(OUT) :: CCPBD1 !! Updated deeper-bed/deposited-material concentration.
      DOUBLEPRECISION, INTENT(IN) :: CCPBS  !! Old bed-surface concentration.
      DOUBLEPRECISION, INTENT(OUT) :: CCPBS1 !! Updated bed-surface concentration.
      DOUBLEPRECISION, INTENT(IN) :: CCPSF  !! Old stream-water concentration.
      DOUBLEPRECISION, INTENT(OUT) :: CCPSF1 !! Updated stream-water concentration.
      DOUBLEPRECISION, INTENT(IN) :: TSE    !! Scaled contaminant timestep.
      INTEGER :: nc, nk, njda
      DOUBLEPRECISION :: duma1, duma2, duma3, duma4, duma5, duma6, duma7, dump5, dump6
      DOUBLEPRECISION :: dumb1, dumb2, dumb3, dumb3a, dumb3b
      DOUBLEPRECISION :: dump1, dump2, dump3, dump4, dump7, dsum, sum
      DOUBLEPRECISION :: sum1, sum2, sum3, sum4, sum5, alt, altstr
      DOUBLEPRECISION :: blt, bltstr, dlt, dltstr, elt, eltstr, eltda, dltda, dumf1
      DOUBLEPRECISION :: flt, fltstr, fltda, hlt, hltstr, hltda
      DOUBLEPRECISION :: plt, dumq1, qlt, qltda, slt, sltda, gylt, gyltda, gyltsr
      DOUBLEPRECISION :: wmesf, wmebs, wmebd
      DUMA1 = (MAX (zero, - PCSFP1) + MAX (zero, - PCSFM1) ) &
         / KS
      DUMA2 = FCPSF + TSE * FCPSFT + FCPSFC * CCPSF
      DUMA3 = MAX (zero, WCPBD1)
      DUMA4 = FCPSD+TSE * FCPSDT + FCPSDC * CCPSF
      SUM3 = zero
      SUM4 = zero
      SUM5 = zero
      DO 1 NK = 1, 2
         SUM1 = zero
         SUM2 = zero
         DO 2 NC = NCEBK (NK), NCETOP
            SUM1 = SUM1 + MAX (zero, - PCPBK1 (NK, NC) )
            SUM2 = SUM2 + MAX (zero, PCPBK1 (NK, NC) ) * CCPBK (NK, &
               NC)
            SUM3 = SUM3 + (FCPBK (NK, NC) * CCPBK (NK, NC) + GCPBK (NK, &
               NC) * SCPBK (NK, NC) ) * KSPBK (NK, NC)
2        END DO
         SUM4 = SUM4 + SUM1
         SUM5 = SUM5 + SUM2
1     END DO
      DUMA5 = SUM4
      DUMP5 = VCPBK1 * SUM3
      DUMP6 = SUM5
      DUMA6 = MAX (zero, - PCPSW1 (1) ) + MAX (zero, - PCPSW1 (2) &
         )
      DUMA7 = MAX (zero, - PCPSB1 (1) ) + MAX (zero, - PCPSB1 (2) &
         )
      ALT = ACPSF1 * (one + TSE * (DUMA1 + GCPLAL) ) * DUMA2 + TSE * &
         (DUMA3 * ACPBD1 * DUMA4 + ICSBSC + ECPSFC) + TSE * (DUMA5 + DUMA6 &
         * DUMA2 + DUMA7)


      ALTSTR = TSE * ( (ACPSF1 * (one + TSE * (DUMA1 + GCPLAL) ) &
         + TSE * DUMA6) * FCPSFC + TSE * DUMA3 * ACPBD1 * FCPSDC)
!                             SET a AND a*
      DUMB1 = MAX (zero, - WCPBD1)
      DUMB2 = FCPBS + TSE * FCPBST + FCPBSC * CCPBS
      DUMB3A = MAX (zero, PCPSB1 (1) )
      DUMB3B = MAX (zero, PCPSB1 (2) )
      DUMB3 = DUMB3A + DUMB3B
      BLT = - TSE * (DUMB1 * ACPBD1 * DUMB2 + DUMB3)


      BLTSTR = - TSE * TSE * DUMB1 * ACPBD1 * FCPBSC
!                             SET b AND b*
      DLT = - TSE * (DUMA3 * ACPBD1 * DUMA4 + ICSBSC + DUMA7)
      DLTSTR = - TSE * TSE * DUMA3 * ACPBD1 * FCPSDC


      DLTDA = - TSE * USCP * ACSBS1
!                             SET d, d*, AND d'
      ELT = (ACPBS + TSE * (GCPLAL * ACPBS + (DUMA3 + DUMB1) * ACPBD1) ) &
         * DUMB2 + TSE * (ICSBDC + ECPBSC + DUMA7 + DUMB3)
      ELTSTR = TSE * (ACPBS + TSE * (GCPLAL * ACPBS + (DUMA3 + DUMB1) &
         * ACPBD1) ) * FCPBSC


      ELTDA = TSE * (USCP * ACSBS1 + ACSBD1)
!                             SET e, e*, AND e'
      DUMF1 = FCPBD+TSE * FCPBDT + FCPBDC * CCPBD
      FLT = - TSE * (DUMB1 * ACPBD1 * DUMF1 + DUMB3)
      FLTSTR = - TSE * TSE * DUMB1 * ACPBD1 * FCPBDC

      FLTDA = - TSE * ACSBD1
!                             SET f, f*, AND f'
      HLT = - TSE * (DUMA3 * ACPBD1 * DUMB2 + DUMA7 + ICSBDC)
      HLTSTR = - TSE * TSE * DUMA3 * ACPBD1 * FCPBSC


      HLTDA = - TSE * ACSBD1
!                             SET h, h*, AND h'
      DUMP1 = (FCPSF + TSE * FCPSFT) * CCPSF
      DUMP2 = (FCPSD+TSE * FCPSDT) * CCPSF
      DUMP3 = (FCPBS + TSE * FCPBST) * CCPBS
      DSUM = zero
      SUM = zero
      SUM1 = zero
      IF (PCSFM1.GT.0) THEN
         DO 3 NJDA = 1, 3
            SUM = SUM + ACSFA1 (NJDA) * MAX (zero, PCSFA1 (NJDA) ) &
               * FCSFA1 (NJDA) * CCSFA1 (NJDA)
            SUM1 = SUM1 + ACSFA1 (NJDA) * PCSFA1 (NJDA)
3        END DO
         IF (NOTZERO(SUM1)) DSUM = ACPSF1 * PCSFM1 * SUM / SUM1
      ENDIF
      SUM = zero
      SUM1 = zero
      IF (PCSFP1.GT.0) THEN
         DO 5 NJDA = 4, 6
            SUM = SUM + ACSFA1 (NJDA) * MAX (zero, PCSFA1 (NJDA) ) &
               * FCSFA1 (NJDA) * CCSFA1 (NJDA)
            SUM1 = SUM1 + ACSFA1 (NJDA) * PCSFA1 (NJDA)
5        END DO
         IF (NOTZERO(SUM1)) DSUM = DSUM + ACPSF1 * PCSFP1 * SUM / SUM1
      ENDIF
      DUMP4 = DSUM / KS
      SUM = zero
      DO 4 NK = 1, 2
         SUM = SUM + MAX (zero, PCPSW1 (NK) ) * FCPSW1 (NK) * CCPGS1 &
            (NK)
4     END DO
      DUMP7 = SUM


      PLT = - (ACPSF1 * FCPSFT + ACPSFT * FCPSF) * CCPSF - ACPSF1 * &
         (DUMA1 + GCPLAL) * DUMP1 + DUMP4 + DUMP5 - DUMA3 * ACPBD1 * DUMP2 &
         + DUMB1 * ACPBD1 * DUMP3 - USCP * (QCP1 + ICP1) - ICPSBS - TSE * &
         ICSBST + GCPLAQ * ACPSF1 * FCSF1Q * CCSF1Q - ECPSF - TSE * ECPSFT &
         + DUMP6 - DUMA5 * CCPSF + DUMP7 - DUMA6 * DUMP1 + DUMB3 * CCPBS - &
         DUMA7 * CCPSF
!                                       SET p
      DUMQ1 = (FCPBD+TSE * FCPBDT) * CCPBD
      QLT = - ACPBS * FCPBST * CCPBS - (GCPLAL * ACPBS + (DUMA3 + DUMB1) &
         * ACPBD1) * DUMP3 + DUMA3 * ACPBD1 * DUMP2 + DUMB1 * ACPBD1 * &
         DUMQ1 - (one - USCP) * (QCP1 + ICP1) + ICPSBS - ICPSBD-ECPBS + &
         TSE * (ICSBST - ICSBDT - ECPBST) + GCPLAQ * ACPBS * FCBS1Q * &
         CCBS1Q + DUMB3 * (CCPBD-CCPBS) - DUMA7 * (CCPBS - CCPSF)


      QLTDA = USCP * ACSBS1 * (CCPSF - CCPBS) - ACSBD1 * (CCPBS - CCPBD)
!                                       SET q ANS q'
      SLT = - (ACPBD1 * FCPBDT + ACPBDT * FCPBD) * CCPBD- ( (GCPLAL + &
         DUMB1) * DUMQ1 - DUMA3 * DUMP3) * ACPBD1 + ICPSBD-ECPBD+TSE * &
         (ICSBDT - ECPBDT) + GCPLAQ * ACPBD1 * FCBD1Q * CCBD1Q + DUMB3A * &
         (CCPBK (1, 1) - CCPBD) + DUMB3B * (CCPBK (2, 1) - CCPBD) - DUMA7 * &
         (CCPBD-CCPBS)


      SLTDA = ACSBD1 * (CCPBS - CCPBD)
!                                       SET s AND s'
      GYLT = ACPBD1 * (one + TSE * (GCPLAL + DUMB1) ) * DUMF1 + TSE * &
         (ECPBDC + DUMA7 + DUMB3)
      GYLTSR = TSE * ACPBD1 * (one + TSE * (GCPLAL + DUMB1) ) * &
         FCPBDC


      GYLTDA = TSE * ACSBD1
!                             SET y, y*, AND Y'
      IF (USCP.LT.half) THEN
         CALL SNL3 (one, zero, zero, zero, zero, - DLT - DLTDA, &
            - DLTSTR, ELT + ELTDA, ELTSTR, - FLT - FLTDA, - FLTSTR, &
            - HLT - HLTDA, - HLTSTR, zero, QLT + QLTDA, SLT + SLTDA, &
            WMESF, WMEBS, WMEBD, GYLT + GYLTDA, GYLTSR)
!                             SPECIAL CASE: NO WATER IN LINK
      ELSE
         CALL SNL3 (ALT - DLTDA, ALTSTR, - BLT + ELTDA + HLTDA, - &
            BLTSTR, zero, - DLT - DLTDA, - DLTSTR, ELT + ELTDA, ELTSTR, &
            - FLT - FLTDA, - FLTSTR, - HLT - HLTDA, - HLTSTR, PLT - QLTDA - &
            SLTDA, QLT + QLTDA, SLT + SLTDA, WMESF, WMEBS, WMEBD, GYLT + &
            GYLTDA, GYLTSR)
      ENDIF
      CCPBD1 = CCPBD+TSE * WMEBD
      CCPBS1 = CCPBS + TSE * WMEBS

      CCPSF1 = CCPSF + TSE * WMESF
!                             SOLVE THE DIFFERENCE EQUATIONS
!                             AND UPDATE THE CONCENTRATIONS
      RETURN
   END SUBROUTINE LINK
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!

! 12/8/94




!> Returns the fraction of soil water treated as mobile.
!>
!> The current implementation returns the placeholder constant `0.5`. Although
!> `CMRD` reads the manual field `CM57` into a local soil mobile-water-fraction
!> table, those values are not yet used here.
   DOUBLEPRECISION FUNCTION PHI (JSOIL, THETA)
!                             FRACTION OF SOIL WATER WHICH IS MOBILE
      INTEGER, INTENT(IN) :: JSOIL !! Soil data-set index.
      DOUBLEPRECISION, INTENT(IN) :: THETA !! Soil water content.
      PHI = half
!                             ########## SOIL INFO NEEDED HERE #########






   END FUNCTION PHI



!> Updates plant uptake and plant concentrations for one column and contaminant.
!>
!> `PLCOLM` distributes uptake over rooted cells using plant/root fractions,
!> accumulates dissolved and sorbed uptake source terms for [[colm]], and calls
!> [[plant]] to update the two plant compartments for each plant type.
!>
!> For the first contaminant in a chain, plant generation terms `GENAA` and
!> `GENBB` are reset before uptake is accumulated. For every plant type present
!> in column `NCL`, the rooted interval is
!>
!> \[
!> NCE = NCETOP-NRD(JPLTY),\ldots,NCETOP.
!> \]
!>
!> In each rooted cell the mobile and immobile concentration contributions used
!> for uptake are split with the mobile-water fraction `PPHI` and plant uptake
!> weighting `XXI`:
!>
!> \[
!> C_d = XXI\,PPHI\,COLCAP,\qquad
!> C_s = (1-XXI\,PPHI)\,SOLCAP,\qquad
!> C_t = C_d + C_s.
!> \]
!>
!> The potential cell uptake contribution for plant `p` is
!>
!> \[
!> U_{k,p} =
!> Z2SQOD\,
!> \frac{PFTWO(JPLTY)}{PF2MAX(JPLTY)}\,
!> PKMAX(JPLTY,NCONT)\,
!> PDZF3(NCL,k,p)\,C_t,
!> \]
!>
!> where `PDZF3` is the rooted-cell distribution and `PKMAX` is the maximum
!> uptake parameter for the contaminant and plant type. The dissolved and
!> sorbed uptake terms supplied to [[colm]] are accumulated over all plant
!> types as
!>
!> \[
!> EDCAP_k \mathrel{+}= C_d\,
!> \frac{U_{k,p}\,PFONE(NCL,p)}
!>      {C_t\,Z2\,KSP_k},\qquad
!> ESCAP_k \mathrel{+}= C_s\,
!> \frac{U_{k,p}\,PFONE(NCL,p)}
!>      {C_t\,Z2\,KSP_k}.
!> \]
!>
!> The total plant uptake is partitioned between the two plant compartments
!> using the legacy plant parameters
!> `DELONE`, `DELTWO`, `DELTHR`, and `DELFOU`:
!>
!> \[
!> Q =
!> \frac{\sum_k U_{k,p}}
!>      {PMASS\left((1-DELONE) + DELTHR\,DELONE\,PFTWO/PF2MAX\right)},
!> \]
!>
!> \[
!> QCPAA = (1-DELONE)Q,\qquad
!> QCPBB = DELTHR\,DELONE\,\frac{PFTWO}{PF2MAX}\,Q.
!> \]
!>
!> If plant compartment-B mass decreases during the timestep, `PLCOLM` returns
!> that released contaminant to the soil uptake/source term, weighted by
!> `DELTWO` and the root distribution. The two-compartment plant balance itself
!> is then solved by [[plant]], and the updated `BCPAA` and `BCPBB`
!> concentrations are stored for the column, plant type, and contaminant.
!>
!> | Condition | Effect |
!> |:----------|:-------|
!> | `NCONT=1` | Reset plant generation terms `GENAA` and `GENBB` before processing the contaminant chain. |
!> | `GMCBBD < 0` | Return compartment-B loss to the soil dynamic-region source term `EDCAP`. |
!>
!> @warning The uptake calculation divides by \(C_t\). The code assumes rooted
!> cells have nonzero total plant-available concentration; no zero guard is
!> applied before `EDDUM = DUM*F1DUM/(TDUM*Z2*KSP)`.
!> @endwarning
   SUBROUTINE PLCOLM (NCL, NCONT)

      USE CONT_CC
      USE COLM_C1
!                 NB COLM.C1 includes AL.P
      USE COLM_C2

      USE COLM_CC

      USE PLANT_CC
!                 Include parameter statements, water/contaminant
!                 interface COMMON blocks, and plant COMMON blocks
!                 called just before routine COLM
      INTEGER, INTENT(IN) :: NCL   !! Land-column element being updated.
      INTEGER, INTENT(IN) :: NCONT !! Contaminant index being updated.
      INTEGER :: jplant, nce, jplty, nrbot
      DOUBLEPRECISION :: d1dum, d2dum, d3dum, d4dum, o2dum, f1dum, f2dum, pkdum, &
         pmdum, sum, z2dum, xdum, cdum, sdum, tdum, dum, eddum, qdum, bcdum, dum1, dum3, bcpaa1, bcpbb1
      IF (NCONT.EQ.1) THEN
         DO 100 JPLANT = 1, NPL (NCL)
            GENAA (JPLANT) = zero
            GENBB (JPLANT) = zero
100      END DO

      ENDIF
!                 Set generation variables to zero if call is for first
!                 contaminant
      DO 900 NCE = 1, NCETOP
         EDCAP (NCE) = zero
         EDCAPC (NCE) = zero
         EDCAPT (NCE) = zero
         ESCAP (NCE) = zero
         ESCAPS (NCE) = zero
         ESCAPT (NCE) = zero



900   END DO
!                 Set uptake variables to zero in preparation for
!                 summing net uptake over all plant types on
!                 column NCL
!                 Main calculation loops
      DO 1000 JPLANT = 1, NPL (NCL)
!                 For each plant type on soil column NCL
         JPLTY = NPLTYP (NCL, JPLANT)
!                 Plant type number

         NRBOT = NCETOP - NRD (JPLTY)
!                 Number of bottom rooted cell
         D1DUM = DELONE (JPLTY)
         D2DUM = DELTWO (JPLTY)
         D3DUM = DELTHR (JPLTY)
         D4DUM = DELFOU (JPLTY)
         O2DUM = one - D2DUM
         F1DUM = PFONE (NCL, JPLANT)
         F2DUM = PFTWO (JPLTY) / PF2MAX (JPLTY)
         PKDUM = PKMAX (JPLTY, NCONT)

         PMDUM = PMASS (JPLTY)
         GCPL = GCPLA (NCONT)
!                 Non dimensioned decay variable, set up in MUZ
         GMCPAA = (one - D1DUM)

         GMCPBB = F2DUM * D1DUM
         SUM = zero
         Z2DUM = Z2SQOD * F2DUM * PKDUM
         DO 1610 NCE = NRBOT, NCETOP
            XDUM = XXI * PPHI (NCE)
            CDUM = XDUM * COLCAP (NCE)
            SDUM = (one - XDUM) * SOLCAP (NCE)
            TDUM = CDUM + SDUM
            DUM = Z2DUM * PDZF3 (NCL, NCE, JPLANT) * TDUM
            SUM = SUM + DUM
            EDDUM = DUM * F1DUM / (TDUM * (Z2 * KSP (NCE) ) )
            EDCAP (NCE) = EDCAP (NCE) + CDUM * EDDUM
            ESCAP (NCE) = ESCAP (NCE) + SDUM * EDDUM
!                  Set net scaled uptake rates for use in routine COLM
!            ----- NB sums up over all plant types
!            ----- NB THE RECYLING TERMS FOR EDCAP AND ESCAP ARE
!                    ADDED BELOW
1610     END DO
         QDUM = SUM / (PMDUM * (GMCPAA + (D3DUM * GMCPBB) ) )
         QCPAA = GMCPAA * QDUM

         QCPBB = D3DUM * GMCPBB * QDUM
!                 Evaluate scaled values for Qa and Qb using
!                 equations in section 3 of WRSRU/TR/9107/12
         GMCBBD = (GMCPBB - GMCBBO (NCL, JPLANT) ) / TSE
         GMCBBO (NCL, JPLANT) = GMCPBB
         IF (LTZERO(GMCBBD)) THEN
            BCDUM = BCPBB (NCL, JPLANT, NCONT)
            DUM1 = F1DUM * D4DUM * BCDUM * GMCBBD
            DUM3 = O2DUM * PDZF3 (NCL, NCETOP, JPLANT)
            EDCAP (NCETOP) = EDCAP (NCETOP) + DUM1 * (D2DUM + DUM3) &
               / (Z2 * KSP (NCETOP) * RHOPL)
            DO 1630 NCE = NRBOT, NCETOP - 1
               EDCAP (NCE) = EDCAP (NCE) + DUM1 * DUM3 / (Z2 * KSP (NCE) &
                  * RHOPL)
1630        END DO

         ENDIF
         CALL PLANT (JPLANT, BCPAA (NCL, JPLANT, NCONT), BCPAA1, BCPBB ( &
            NCL, JPLANT, NCONT), BCPBB1, TSE)
         BCPAA (NCL, JPLANT, NCONT) = BCPAA1

         BCPBB (NCL, JPLANT, NCONT) = BCPBB1
!                 Call solve routine and update concentrations

1000  END DO
      RETURN

   END SUBROUTINE PLCOLM




!> Solves the column contaminant linear system.
!>
!> `SLVCLM` reduces the coupled mobile/immobile column equations to a
!> tridiagonal system, solves it with `TRIDAG`, and iterates coefficient updates
!> when nonlinear adsorption is active.
!>
!> For each column cell \(i\), [[colm]] has assembled two coupled equations for
!> the mobile-region rate \(\Omega_i\) and immobile/dead-space rate
!> \(\epsilon_i\):
!>
!> \[
!> FLT_i\Omega_{i-1} + ELT_i\Omega_i + DLT_i\Omega_{i+1}
!>      - GLT_i\epsilon_i = SLT_i,
!> \]
!>
!> \[
!> PLT_i\epsilon_i - TLT_i\Omega_i = QLT_i.
!> \]
!>
!> `SLVCLM` eliminates the immobile-rate unknown with
!>
!> \[
!> \epsilon_i = \frac{QLT_i + TLT_i\Omega_i}{PLT_i}
!> \]
!>
!> and therefore solves the reduced tridiagonal mobile-rate system
!>
!> \[
!> FLT_i\Omega_{i-1}
!> + \left(ELT_i-\frac{GLT_iTLT_i}{PLT_i}\right)\Omega_i
!> + DLT_i\Omega_{i+1}
!> =
!> SLT_i+\frac{GLT_iQLT_i}{PLT_i}.
!> \]
!>
!> The reduced diagonal and right-hand side are stored in local work arrays as
!>
!> \[
!> ELTE_i = ELT_i-GLT_iTLT_i/PLT_i,\qquad
!> RHTD_i = SLT_i+GLT_iQLT_i/PLT_i.
!> \]
!>
!> `TRIDAG` returns \(\Omega\), after which \(\epsilon\) is reconstructed from
!> the eliminated equation. If nonlinear adsorption is active (`ISADNL`), the
!> routine performs ten Picard-style coefficient updates using the current
!> rates:
!>
!> \[
!> PLTE_i = PLT_i + PLTSTR_i\epsilon_i,
!> \]
!>
!> \[
!> ELTE_i = ELT_i + ELTSTR_i\Omega_i
!>          - \frac{GLT_iTLT_i}{PLTE_i},\qquad
!> RHTD_i = SLT_i + \frac{GLT_iQLT_i}{PLTE_i}.
!> \]
!>
!> Each update repeats the tridiagonal solve for \(\Omega\) and reconstructs
!> \(\epsilon_i=(QLT_i+TLT_i\Omega_i)/PLTE_i\). Convergence is not tested here;
!> the fixed ten iterations reproduce the legacy nonlinear adsorption solve.
!>
!> | Adsorption mode | Solve path |
!> |:----------------|:-----------|
!> | Linear (`ISADNL=.FALSE.`) | One tridiagonal solve, then reconstruct `EPS` with the original `PLT`. |
!> | Nonlinear (`ISADNL=.TRUE.`) | Initial solve plus ten fixed coefficient-update iterations using `ELTSTR` and `PLTSTR`. |
   SUBROUTINE SLVCLM (N)

      USE COLM_CC1
      INTEGER, INTENT(IN) :: n !! Number of active column cells in the reduced system.
      INTEGER :: na, loop
      DOUBLEPRECISION ELTE (LLEE), PLTE (LLEE), RHTD (LLEE)
!                            ALLOCATE WORKSPACE
      DO 1 NA = 1, N
         ELTE (NA) = ELT (NA) - GLT (NA) * TLT (NA) / PLT (NA)
         RHTD (NA) = SLT (NA) + GLT (NA) * QLT (NA) / PLT (NA)
1     END DO
      CALL TRIDAG (FLT, ELTE, DLT, RHTD, OME, N)
      DO 2 NA = 1, N
         EPS (NA) = (QLT (NA) + TLT (NA) * OME (NA) ) / PLT (NA)

2     END DO
!                            ESTIMATE OMEGA AND EPSILON VECTORS
      IF (ISADNL) THEN
!                            GO ROUND LOOP 3 ONLY IF
!                            THERE IS NONLINEAR ADSORPTION
         DO 3 LOOP = 1, 10
            DO 4 NA = 1, N
               PLTE (NA) = PLT (NA) + PLTSTR (NA) * EPS (NA)
               ELTE (NA) = ELT (NA) + ELTSTR (NA) * OME (NA) - GLT (NA) &
                  * TLT (NA) / PLTE (NA)
               RHTD (NA) = SLT (NA) + GLT (NA) * QLT (NA) / PLTE (NA)


4           END DO
!                            SET 'NON-LINEAR' COEFFICIENTS


            CALL TRIDAG (FLT, ELTE, DLT, RHTD, OME, N)
!                            ESTIMATE OMEGA VECTOR
            DO 5 NA = 1, N
               EPS (NA) = (QLT (NA) + TLT (NA) * OME (NA) ) / PLTE (NA)
5           END DO
!                            ESTIMATE EPSILON VECTOR

3        END DO
      ELSE
         RETURN

      ENDIF
      RETURN

   END SUBROUTINE SLVCLM
! 12/8/94



!> Calculates ground-surface retardation and derivatives.
!>
!> The routine returns the retardation factor `R`, its concentration derivative
!> `RC`, and its time derivative `RT`. It depends on surface-water concentration
!> `C`, old and new sediment particle-size fractions `FRNO` and `FRN`, the
!> Freundlich power `GN`, old and new moisture contents `THO` and `TH`, reference
!> distribution coefficients `KDREF`, scaled timestep `DT`, number of sediment
!> fractions `NSED`, and the nonlinear-adsorption flag `ISNL`. With nonlinear
!> adsorption enabled, the concentration dependence follows the Freundlich-type
!> exponent `GN`.
!>
!> The old and new particle-size-weighted distribution coefficients are
!>
!> \[
!> K_o = \sum_j FRNO_j\,KDREF_j,\qquad
!> K_n = \sum_j FRN_j\,KDREF_j.
!> \]
!>
!> For linear adsorption (`ISNL = .FALSE.`), the retardation factor is evaluated
!> from the old state and the time derivative is the finite-difference change
!> from old to new sediment/moisture conditions:
!>
!> \[
!> R = 1 + \frac{K_o}{THO},\qquad
!> R_T = \frac{K_n/TH - K_o/THO}{DT},\qquad
!> R_C = 0.
!> \]
!>
!> For nonlinear Freundlich adsorption, the concentration dependence is
!> linearised about the current concentration:
!>
!> \[
!> D_o = \frac{K_o}{THO} C^{GN-2},\qquad
!> D_n = \frac{K_n}{TH} C^{GN-2},
!> \]
!>
!> \[
!> R = 1 + D_o C,\qquad
!> R_T = \frac{(D_n-D_o)C}{DT},\qquad
!> R_C = (GN-1)D_o.
!> \]
!>
!> These returned quantities are used by the column and surface/link assembly
!> routines to include concentration- and time-dependent sorption storage in the
!> implicit contaminant equations.
!>
!> @warning In the nonlinear branch, `RET` evaluates `C**(GN-2)` directly. Unlike
!> [[fret]], it does not special-case `C=0`; callers must only use nonlinear
!> surface retardation where that power operation is valid.
   subroutine RET (C, GN, THO, TH, FRNO, FRN, KDREF, R, RC, RT, DT, &
      NSED, ISNL)

!      IMPLICIT DOUBLEPRECISION (A-H,O-Z)
      DOUBLEPRECISION, INTENT(IN) :: C       !! Surface-water concentration.
      DOUBLEPRECISION, INTENT(IN) :: GN      !! Freundlich power.
      DOUBLEPRECISION, INTENT(IN) :: THO     !! Old moisture content.
      DOUBLEPRECISION, INTENT(IN) :: TH      !! New moisture content.
      DOUBLEPRECISION, INTENT(OUT) :: R      !! Retardation factor.
      DOUBLEPRECISION, INTENT(OUT) :: RC     !! Concentration derivative of `R`.
      DOUBLEPRECISION, INTENT(OUT) :: RT     !! Time derivative contribution for `R`.
      DOUBLEPRECISION, INTENT(IN) :: DT      !! Scaled timestep.
      DOUBLEPRECISION, INTENT(IN) :: FRNO(*) !! Old sediment particle-size fractions.
      DOUBLEPRECISION, INTENT(IN) :: FRN(*)  !! New sediment particle-size fractions.
      DOUBLEPRECISION, INTENT(IN) :: KDREF(*) !! Reference distribution coefficients.
      INTEGER, INTENT(IN) :: NSED !! Number of sediment size fractions.
      INTEGER :: jsed, nj
      DOUBLEPRECISION :: dumo, dum, sumo, sum, cdum, dumko, dumk, x1min

      LOGICAL, INTENT(IN) :: ISNL !! True for nonlinear adsorption.
      DUMO = one / THO
      DUM = one / TH
      SUMO = zero
      SUM = zero
      DO 1 JSED = 1, NSED
         SUMO = SUMO + FRNO (JSED) * KDREF (JSED)
         SUM = SUM + FRN (JSED) * KDREF (JSED)
1     END DO
      IF (.NOT.ISNL) THEN
!                             IS LINEAR ADSORPTION
         R = one + SUMO * DUMO
         RT = (SUM * DUM - SUMO * DUMO) / DT
         RC = zero
      ELSE
         CDUM = C** (GN - two)
         DUMKO = SUMO * DUMO * CDUM
         DUMK = SUM * DUM * CDUM
         R = one + DUMKO * C
         RT = (DUMK - DUMKO) * C / DT
         RC = (GN - one) * DUMKO
      ENDIF
      return
   end subroutine RET



!> Solves a three-variable nonlinear link concentration system by fixed-point iteration.
!>
!> `SNL3` is used by [[link]] for the coupled bed, bed-surface, and surface-water
!> concentration equations when nonlinear coefficient terms are present. It
!> finds roots of the coupled nonlinear stream difference-equation system:
!>
!> \[
!> \begin{aligned}
!> (A + AS X_1)X_1 - (B + BS X_2)X_2 - C X_3 &= P,\\
!> -(D + DS X_1)X_1 + (E + ES X_2)X_2 - (F + FS X_3)X_3 &= Q,\\
!> -(H + HS X_2)X_2 + (AY + AYS X_3)X_3 &= S.
!> \end{aligned}
!> \]
!>
!> The solver starts from \(X_1=X_2=X_3=0\) and applies 100 fixed-point
!> iterations:
!>
!> \[
!> X_1 \leftarrow
!> \frac{P + (B+BSX_2)X_2 + C X_3}{A+ASX_1},
!> \]
!>
!> \[
!> X_2 \leftarrow
!> \frac{Q + (D+DSX_1)X_1 + (F+FSX_3)X_3}{E+ESX_2},
!> \]
!>
!> \[
!> X_3 \leftarrow
!> \frac{S + (H+HSX_2)X_2}{AY+AYSX_3}.
!> \]
!>
!> After the main iteration, the routine checks that the solution lies inside
!> the legacy convergence region. When nonlinear denominators are present, the
!> lower bounds are
!>
!> \[
!> X_{1,\min} =
!> \frac{-A + |B+2BSX_2| + C}{2AS},
!> \]
!>
!> \[
!> X_{2,\min} =
!> \frac{-E + |D+2DSX_1| + |F+2FSX_3|}{2ES},
!> \]
!>
!> \[
!> X_{3,\min} =
!> \frac{-AY + |H+2HSX_2|}{2AYS}.
!> \]
!>
!> If the corresponding nonlinear coefficient is zero, the current value is
!> used as the bound. Values below these bounds produce the legacy
!> `FATAL CONVERGENCE ERROR 1` diagnostic.
!>
!> | Diagnostic | Check |
!> |:-----------|:------|
!> | Error 1 | Solution lies below the computed convergence-region lower bounds. |
!> | Error 2 | Three additional fixed-point steps still change the solution by more than the tolerance. |
!> | Error 3 | Substituting the final solution into the three original equations leaves a residual sum at least \(10^{-2}\). |
!>
!> The routine then performs three further fixed-point steps and compares the
!> total change with
!>
!> \[
!> X_{ref}=|X_1|+|X_2|+|X_3|.
!> \]
!>
!> A relative change above \(10^{-2}\) produces `FATAL CONVERGENCE ERROR 2`.
!> In the implemented Fortran expression, operator precedence means only
!> `ABS(X3-X3OLD)` is divided by `XREF`; the `X1` and `X2` changes are added
!> without that scaling.
!> Finally, the three residuals of the original equations are recomputed and
!> normalised by \(P\), \(Q\), and \(S\) when these are nonzero; if
!>
!> \[
!> |P_{err}| + |Q_{err}| + |S_{err}| \ge 10^{-2},
!> \]
!>
!> the routine prints `CONVERGENCE ERROR 3 IN SNL3`, suppressing repeated
!> messages after the tenth occurrence.
   SUBROUTINE SNL3 (A, AS, B, BS, C, D, DS, E, ES, F, FS, H, HS, P, &
      Q, S, X1, X2, X3, AY, AYS)
      INTEGER :: nj, njtest
      DOUBLEPRECISION, INTENT(IN) :: A   !! Coefficient multiplying `X1` in equation 1.
      DOUBLEPRECISION, INTENT(IN) :: AS  !! Nonlinear coefficient multiplying `X1**2` in equation 1.
      DOUBLEPRECISION, INTENT(IN) :: B   !! Coefficient multiplying `X2` in equation 1.
      DOUBLEPRECISION, INTENT(IN) :: BS  !! Nonlinear coefficient multiplying `X2**2` in equation 1.
      DOUBLEPRECISION, INTENT(IN) :: C   !! Coefficient multiplying `X3` in equation 1.
      DOUBLEPRECISION, INTENT(IN) :: D   !! Coefficient multiplying `X1` in equation 2.
      DOUBLEPRECISION, INTENT(IN) :: DS  !! Nonlinear coefficient multiplying `X1**2` in equation 2.
      DOUBLEPRECISION, INTENT(IN) :: E   !! Coefficient multiplying `X2` in equation 2.
      DOUBLEPRECISION, INTENT(IN) :: ES  !! Nonlinear coefficient multiplying `X2**2` in equation 2.
      DOUBLEPRECISION, INTENT(IN) :: F   !! Coefficient multiplying `X3` in equation 2.
      DOUBLEPRECISION, INTENT(IN) :: FS  !! Nonlinear coefficient multiplying `X3**2` in equation 2.
      DOUBLEPRECISION, INTENT(IN) :: H   !! Coefficient multiplying `X2` in equation 3.
      DOUBLEPRECISION, INTENT(IN) :: HS  !! Nonlinear coefficient multiplying `X2**2` in equation 3.
      DOUBLEPRECISION, INTENT(IN) :: P   !! Right-hand side of equation 1.
      DOUBLEPRECISION, INTENT(IN) :: Q   !! Right-hand side of equation 2.
      DOUBLEPRECISION, INTENT(IN) :: S   !! Right-hand side of equation 3.
      DOUBLEPRECISION, INTENT(OUT) :: X1 !! Solved variable for equation 1.
      DOUBLEPRECISION, INTENT(OUT) :: X2 !! Solved variable for equation 2.
      DOUBLEPRECISION, INTENT(OUT) :: X3 !! Solved variable for equation 3.
      DOUBLEPRECISION, INTENT(IN) :: AY  !! Coefficient multiplying `X3` in equation 3.
      DOUBLEPRECISION, INTENT(IN) :: AYS !! Nonlinear coefficient multiplying `X3**2` in equation 3.
      DOUBLEPRECISION :: x1min, x2min, x3min, x1old, x2old, x3old, xref, perr, qerr, serr
      X1 = zero
      X2 = zero
      X3 = zero
      DO 1 NJ = 1, 100
         X1 = (P + (B + BS * X2) * X2 + C * X3) / (A + AS * X1)
         X2 = (Q + (D+DS * X1) * X1 + (F + FS * X3) * X3) / (E+ES * X2)
         X3 = (S + (H + HS * X2) * X2) / (AY + AYS * X3)

1     END DO
!                             CHECK SOLUTION IS WITHIN THE CONVERGENCE
!                             REGION
      IF (ISZERO(AS)) THEN
         X1MIN = X1
      ELSE
         X1MIN = ( - A + DABS (B + two * BS * X2) + C) / (two * AS)
      ENDIF
      IF (ISZERO(ES)) THEN
         X2MIN = X2
      ELSE
         X2MIN = ( - E+DABS (D+two * DS * X1) + DABS (F + two * FS * &
            X3) ) / (two * ES)
      ENDIF
      IF (ISZERO(AYS)) THEN
         X3MIN = X3
      ELSE
         X3MIN = ( - AY + DABS (H + two * HS * X2) ) / (two * AYS)
      ENDIF
      IF ( (X1.LT.X1MIN) .OR. (X2.LT.X2MIN) .OR. (X3.LT.X3MIN) ) THEN
         PRINT '(A40)', ' LINK: FATAL CONVERGENCE ERROR 1 IN SNL3'
         PRINT '(A33)', '       ^^^^^^^^^^^^^^^^^^^^^^^^^'


      ENDIF
!                             RUN THREE FURTHER ITERATION STEPS TO SEE
!                             IF THE SOLUTION IS STABLE
      X1OLD = X1
      X2OLD = X2
      X3OLD = X3
      DO 2 NJTEST = 1, 3
         X1 = (P + (B + BS * X2) * X2 + C * X3) / (A + AS * X1)
         X2 = (Q + (D+DS * X1) * X1 + (F + FS * X3) * X3) / (E+ES * X2)

         X3 = (S + (H + HS * X2) * X2) / (AY + AYS * X3)
         XREF = ABS (X1) + ABS (X2) + ABS (X3)
         if (NOTZERO(xref)) then
            IF (ABS (X1 - X1OLD) + ABS (X2 - X2OLD) + ABS (X3 - X3OLD) &
               / XREF.GT.1.0D-2) THEN
               PRINT '(A40)', ' LINK: FATAL CONVERGENCE ERROR 2 IN SNL3'
               PRINT '(A33)', '       ^^^^^^^^^^^^^^^^^^^^^^^^^'
            ENDIF
         endif

2     END DO
!                             CHECK THE SOLUTION IS ACCURATE
      IF (ISZERO(P)) THEN
         PERR = zero
      ELSE
         PERR = ( (A + AS * X1) * X1 - (B + BS * X2) * X2 - C * X3 - P) &
            / P
      ENDIF
      IF (ISZERO(Q)) THEN
         QERR = zero
      ELSE
         QERR = ( - (D+DS * X1) * X1 + (E+ES * X2) * X2 - (F + FS * X3) &
            * X3 - Q) / Q
      ENDIF
      IF (ISZERO(S)) THEN
         SERR = zero
      ELSE
         SERR = ( - (H + HS * X2) * X2 + (AY + AYS * X3) * X3 - S) &
            / S
      ENDIF
      IF ( (ABS (PERR) + ABS (QERR) + ABS (SERR) ) .GE.1.0D-2) THEN
         count = count + 1
         IF (count<10) THEN
            PRINT '(A35)', ' LINK: CONVERGENCE ERROR 3 IN SNL3'
!              PRINT '(A27)' , '       ^^^^^^^^^^^^^^^^^^^'
         ELSEif (COUNT == 10) then
            PRINT '(A)', ' LINK: CONVERGENCE ERROR 3 IN SNL3 - MESSAGES NOW SUPPRESSED'
         ENDIF

      ENDIF
      RETURN
   END subroutine SNL3

! 12/8/94



!> Calculates link retardation and derivatives.
!>
!> `FRET` returns the link retardation factor `F`, its concentration derivative
!> `FC`, and its time derivative `FT`. It depends on concentration `C` in the
!> relevant link cell, Freundlich power `GN`, old and new moisture contents `THO`
!> and `TH`, old and new particle-size fractions `FRNO` and `FRN`, reference
!> distribution coefficients `KDREF`, old and new porosities `PO` and `P`,
!> reference porosity `PREF`, scaled timestep `DT`, number of sediment fractions
!> `NSED`, and the nonlinear-adsorption flag `ISNL`. Compared with [[ret]], it
!> includes the porosity terms required for channel-link compartments.
!>
!> If the concentration is zero, the routine suppresses sorption terms and
!> returns only the water-content storage contribution:
!>
!> \[
!> F = THO,\qquad F_C = 0,\qquad F_T = (TH-THO)/DT.
!> \]
!>
!> Otherwise the porosity correction scales old and new solids fractions to the
!> reference porosity:
!>
!> \[
!> \lambda_o = \frac{1-PO}{1-PREF},\qquad
!> \lambda_n = \frac{1-P}{1-PREF},
!> \]
!>
!> and the particle-size-weighted distribution coefficients are
!>
!> \[
!> K_o = \sum_j FRNO_j\,KDREF_j,\qquad
!> K_n = \sum_j FRN_j\,KDREF_j.
!> \]
!>
!> The old and new sorption/storage multipliers are then
!>
!> \[
!> J_o = \lambda_o K_o,\qquad J_n = \lambda_n K_n.
!> \]
!>
!> For linear adsorption (`ISNL = .FALSE.`), `FRET` returns
!>
!> \[
!> F = THO + J_o,\qquad
!> F_C = 0,\qquad
!> F_T = \frac{TH-THO+J_n-J_o}{DT}.
!> \]
!>
!> For nonlinear Freundlich adsorption, the concentration dependence is
!> linearised as
!>
!> \[
!> D_o = J_o C^{GN-2},\qquad D_n = J_n C^{GN-2},
!> \]
!>
!> giving
!>
!> \[
!> F = TH + D_o C,\qquad
!> F_C = (GN-1)D_o,\qquad
!> F_T = \frac{TH-THO+(D_n-D_o)C}{DT}.
!> \]
!>
!> The returned `F`, `FC`, and `FT` are used by [[linksm]] and [[link]] to
!> linearise dissolved-plus-sorbed contaminant storage in bed, bed-surface,
!> stream-water, and deposited-sediment link compartments.
!>
!> | Case | Returned behaviour |
!> |:-----|:-------------------|
!> | `C=0` | Suppresses sorption terms and returns water-content storage only. |
!> | `C/=0`, linear adsorption | Adds porosity-corrected linear sorption storage. |
!> | `C/=0`, nonlinear adsorption | Adds Freundlich storage and concentration derivative terms. |
   SUBROUTINE FRET (C, GN, THO, TH, FRNO, FRN, KDREF, PO, P, PREF, F, &
      FC, FT, DT, NSED, ISNL)

      DOUBLEPRECISION, INTENT(IN) :: C       !! Link-compartment concentration.
      DOUBLEPRECISION, INTENT(IN) :: GN      !! Freundlich power.
      DOUBLEPRECISION, INTENT(IN) :: THO     !! Old water content.
      DOUBLEPRECISION, INTENT(IN) :: TH      !! New water content.
      DOUBLEPRECISION, INTENT(IN) :: PO      !! Old porosity.
      DOUBLEPRECISION, INTENT(IN) :: P       !! New porosity.
      DOUBLEPRECISION, INTENT(IN) :: PREF    !! Reference porosity for solids scaling.
      DOUBLEPRECISION, INTENT(OUT) :: F      !! Retardation/storage factor.
      DOUBLEPRECISION, INTENT(OUT) :: FC     !! Concentration derivative of `F`.
      DOUBLEPRECISION, INTENT(OUT) :: FT     !! Time derivative contribution for `F`.
      DOUBLEPRECISION, INTENT(IN) :: DT      !! Scaled timestep.
      DOUBLEPRECISION, INTENT(IN) :: FRNO(*) !! Old sediment particle-size fractions.
      DOUBLEPRECISION, INTENT(IN) :: FRN(*)  !! New sediment particle-size fractions.
      DOUBLEPRECISION, INTENT(IN) :: KDREF(*) !! Reference distribution coefficients.
      INTEGER, INTENT(IN) :: NSED            !! Number of sediment size fractions.
      INTEGER :: jsed
      DOUBLEPRECISION :: duma, dumo, dum, sumo, sum, dumjo, dumj, cdum, dumko, dumk

      LOGICAL, INTENT(IN) :: ISNL !! True for nonlinear adsorption.
      IF (ISZERO(C)) THEN
         F = THO
         FC = zero
         FT = (TH - THO) / DT
      ELSE
         DUMA = one / (one - PREF)
         DUMO = (one - PO) * DUMA
         DUM = (one - P) * DUMA
         SUMO = zero
         SUM = zero
         DO 1 JSED = 1, NSED
            SUMO = SUMO + FRNO (JSED) * KDREF (JSED)
            SUM = SUM + FRN (JSED) * KDREF (JSED)
1        END DO
         DUMJO = DUMO * SUMO
         DUMJ = DUM * SUM
         IF (.NOT.ISNL) THEN
!                             IS LINEAR ADSORPTION
            F = THO + DUMJO
            FC = zero
            FT = (TH - THO + DUMJ - DUMJO) / DT
         ELSE
            CDUM = C** (GN - two)
            DUMKO = DUMJO * CDUM
            DUMK = DUMJ * CDUM
            F = TH + DUMKO * C
            FC = (GN - one) * DUMKO
            FT = (TH - THO + (DUMK - DUMKO) * C) / DT
         ENDIF
      ENDIF
      RETURN
   END SUBROUTINE FRET



!> Solves the two-compartment plant contaminant balance for one plant type.
!>
!> The routine sets up and solves the plant difference equations described in
!> WRSRU/TR/9107/12 section 4. The plant model updates compartment A and B
!> concentrations using uptake, plant mass, transfer, decay, and generation
!> terms, returning the updated plant-compartment concentrations in `BCAA1` and
!> `BCBB1`; generation from one contaminant is stored for use by the next
!> contaminant in a chain.
!>
!> `GMCPAA` and `GMCPBB` are the current scaled plant masses for compartments A
!> and B, `QCPAA` and `QCPBB` are the uptake rates supplied by [[plcolm]],
!> `GCPL` is the non-dimensional first-order decay coefficient, and `RHOPL`
!> converts uptake to plant concentration units. The common decay multiplier is
!>
!> \[
!> G = 1 + GCPL\,TSE.
!> \]
!>
!> If compartment A has positive mass, the routine solves for its concentration
!> rate
!>
!> \[
!> W_A =
!> \frac{RHOPL\,QCPAA + GMCPAA\,(GENAA-GCPL\,BCAA)}
!>      {GMCPAA\,G},
!> \]
!>
!> and updates
!>
!> \[
!> BCAA^{n+1} = BCAA^n + TSE\,W_A.
!> \]
!>
!> If compartment A has no mass, `BCAA1` is set to zero. Compartment B uses the
!> same decay/generation structure but also includes the rate of change of
!> compartment-B mass, `GMCBBD`. Before solving,
!>
!> \[
!> T_B = RHOPL\,QCPBB + GMCPBB\,(GENBB-GCPL\,BCBB),
!> \qquad B_B = GMCPBB\,G.
!> \]
!>
!> For increasing or steady B mass (`GMCBBD >= 0`), contaminant dilution/growth
!> is included in the numerator:
!>
!> \[
!> T_B \leftarrow T_B - BCBB\,GMCBBD.
!> \]
!>
!> For decreasing B mass, the implicit denominator is adjusted instead:
!>
!> \[
!> B_B \leftarrow B_B - GMCBBD\,TSE.
!> \]
!>
!> The solved rate and update are
!>
!> \[
!> W_B = T_B/B_B,\qquad BCBB^{n+1}=BCBB^n+TSE\,W_B,
!> \]
!>
!> with `BCBB1` set to zero if the compartment has no mass or the denominator is
!> zero. Finally,
!>
!> \[
!> GENAA \leftarrow GCPL\,BCAA,\qquad
!> GENBB \leftarrow GCPL\,BCBB,
!> \]
!>
!> storing decay-generation terms from the current contaminant for the next
!> contaminant in a chain.
!>
!> | Condition | Result |
!> |:----------|:-------|
!> | `GMCPAA <= 0` | Compartment A concentration is reset to zero. |
!> | `GMCPBB <= 0` | Compartment B concentration is reset to zero. |
!> | `GMCBBD >= 0` | B-mass growth or steady mass reduces the numerator by dilution. |
!> | `GMCBBD < 0` | B-mass loss increases the implicit denominator, limiting concentration growth. |
   SUBROUTINE PLANT (JPLANT, BCAA, BCAA1, BCBB, BCBB1, TSE)

      USE PLANT_CC
      INTEGER, INTENT(IN) :: JPLANT !! Plant type index.
      DOUBLEPRECISION, INTENT(IN) :: BCAA   !! Old plant compartment-A concentration.
      DOUBLEPRECISION, INTENT(OUT) :: BCAA1 !! Updated plant compartment-A concentration.
      DOUBLEPRECISION, INTENT(IN) :: BCBB   !! Old plant compartment-B concentration.
      DOUBLEPRECISION, INTENT(OUT) :: BCBB1 !! Updated plant compartment-B concentration.
      DOUBLEPRECISION, INTENT(IN) :: TSE    !! Scaled contaminant timestep.
      DOUBLEPRECISION :: gdum, wcpaa, topdum, botdum, wcpbb

      GDUM = one + GCPL * TSE
      IF (GTZERO(GMCPAA)) THEN
         WCPAA = (RHOPL * QCPAA + GMCPAA * (GENAA (JPLANT) - GCPL * &
            BCAA) ) / (GMCPAA * GDUM)
         BCAA1 = BCAA + WCPAA * TSE
      ELSE
!                             No plant mass in compartment A
         BCAA1 = zero

      ENDIF
      IF (GTZERO(GMCPBB)) THEN
         TOPDUM = RHOPL * QCPBB + GMCPBB * (GENBB (JPLANT) - GCPL * &
            BCBB)
         BOTDUM = GMCPBB * GDUM
         IF (GEZERO(GMCBBD)) THEN
            TOPDUM = TOPDUM - BCBB * GMCBBD
         ELSE
            BOTDUM = BOTDUM - GMCBBD * TSE
         ENDIF
         IF (NOTZERO(BOTDUM)) THEN
            WCPBB = TOPDUM / BOTDUM
            BCBB1 = BCBB + WCPBB * TSE
         ELSE
            BCBB1 = zero
         ENDIF
      ELSE
         BCBB1 = zero
!                 No mass in compartment B

      ENDIF
      GENAA (JPLANT) = GCPL * BCAA

      GENBB (JPLANT) = GCPL * BCBB
!                 Decay generation values to be used for next
!                 contaminant
      RETURN

   END SUBROUTINE PLANT



!> Prepares plant uptake factors for the current timestep.
!>
!> `PLPREP` updates the plant-type canopy factor used by [[plcolm]] before
!> potential plant contaminant uptake is calculated. For each plant type, the
!> current canopy leaf area index is copied into `PFTWO`:
!>
!> \[
!> PFTWO_p = CLAI_p.
!> \]
!>
!> `PFTWO/PF2MAX` is later used by [[plcolm]] as a canopy-development multiplier
!> on uptake and plant compartment-B mass. The routine also sets `DELFOU`, the
!> factor used when compartment-B mass decreases and contaminant is recycled
!> back to the soil. If canopy is present, the full factor is used:
!>
!> \[
!> DELFOU_p = 1 \qquad \text{when } CLAI_p \ne 0.
!> \]
!>
!> If `CLAI` is zero, `DELFOU` falls back to the configured residual plant
!> fraction:
!>
!> \[
!> DELFOU_p = FLEFT_p \qquad \text{when } CLAI_p = 0.
!> \]
!>
!> This preserves a residual return term for plant material left after canopy
!> loss while allowing active canopy to use the full recycling factor.
   SUBROUTINE PLPREP

      USE PLANT_CC
!                 Include parameter statements, water/contaminant
!                 interface COMMON blocks, and plant COMMON blocks
      INTEGER :: jplty !! Plant type index.
      DO 100 JPLTY = 1, NPLT
         PFTWO (JPLTY) = CLAI (JPLTY)
         IF (NOTZERO(PFTWO (JPLTY))) THEN
            DELFOU (JPLTY) = one
         ELSE
            DELFOU (JPLTY) = FLEFT (JPLTY)
         ENDIF

100   END DO
!                 Set f2 delta4 for each plant type
      RETURN
   END SUBROUTINE PLPREP
end MODULE CMmod
