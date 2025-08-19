MODULE contaminant_data_reader
! RAH  22.03.95  3.4.2  File created 01.02.95.
! SB 1.05.97 See below
! [REFACTORING] 19/08/2025 - Extracted from CMmod.f90 as data reader module
!                           Contains CMRD subroutine for reading contaminant data
!
   USE contaminant_common
   USE CONT_CC, ONLY: CCAPIN

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: cm_read_data

CONTAINS

   !SSSSSS SUBROUTINE CM_READ_DATA (CMD, CPR, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, NEL, NLF, NLFEE, &
   SUBROUTINE cm_read_data (CMD, CPR, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, NEL, NLF, NLFEE, &
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
      !       Version: 4.2  Context: SHETRAN/CM
      ! Modifications:
      ! RAH  22.03.95  3.4.2  File created 01.02.95.
      ! SB 1.05.97 See below
      !----------------------------------------------------------------------*
      !
      ! Input arguments
      INTEGER, INTENT(IN) :: CMD, CPR, MAX_NUM_CATEGORY_TYPES, NCONEE, NELEE, NEL, NLF, NLFEE, NSEE, NS
      INTEGER, INTENT(IN) :: NSEDEE, NSED, MAX_NUM_DATA_PAIRS, NX, NXEE, NYEE, NY
      INTEGER, INTENT(IN) :: ICMXY (NXEE, NY), ICMBK (NLFEE, 2), ICMREF (NELEE, 4, 2:2)
      INTEGER, INTENT(IN) :: NLYRBE (NLF + 1:NEL)
      LOGICAL, INTENT(IN) :: BEXBK, LINKNS (NLFEE)
      !
      ! Output arguments
      INTEGER, INTENT(OUT) :: NCON
      INTEGER, INTENT(OUT) :: NUM_CATEGORIES_TYPES (NCONEE), NCATTY (NELEE, NCONEE)
      INTEGER, INTENT(OUT) :: NCOLMB (NLF + 1:NEL), NTAB (MAX_NUM_CATEGORY_TYPES, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: DBS, DBDI
      DOUBLEPRECISION, INTENT(OUT) :: CCAPI (NCONEE), CCAPE (NELEE, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: CCAPR (NELEE, NCONEE), CCAPB (NELEE, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: TABLE_CONCENTRATION (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: TABLE_WATER_DEPTH (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: IIICF (NCONEE), SOFN (NSEE, 3)
      DOUBLEPRECISION, INTENT(OUT) :: GNN (NCONEE), GGLMSO (NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: ALPHBD (NCONEE), ALPHBS (NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: KDDLS (NSEDEE, NCONEE)
      DOUBLEPRECISION, INTENT(OUT) :: ALPHA (NSEE, NCONEE), FADS (NSEE, NCONEE)
      DOUBLEPRECISION :: PHIDAT (NSEE), DIFDAT (NCONEE), DISPDT (NSEE, NCONEE)
      LOGICAL, INTENT(OUT) :: ISCNSV (NCONEE)
      !
      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT) :: IDUM
      DOUBLEPRECISION, INTENT(INOUT) :: DUMMY (NELEE)
      !
      ! Locals, etc
      INTEGER :: FATAL, rubbish (1, 1), j
      PARAMETER (FATAL = 1)
      !
      INTEGER :: I, IEL, INDX, NC, NCBC, NCED, NCLBND, NCONCM, NCONT
      INTEGER :: NDATA, NFEX, NMAX (3), NREQ, NSCM, NSEDCM, NTB, NTBL, SOIL
      LOGICAL :: LDUM (1)
      CHARACTER (80) :: CDUM(1)
      CHARACTER(132) :: MSG
      !
      !----------------------------------------------------------------------*
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
         IF (NREQ.GT.NELEE) THEN
            WRITE (MSG, 9809) NELEE, NREQ, 'non-default columns', 'CM9: NCLBND ', NCLBND
            CALL ERROR (FATAL, 3001, CPR, 0, 0, MSG)
            RETURN
         ENDIF
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
         IF (IEL.LE.NLF.OR.IEL.GT.NEL) THEN
            WRITE (MSG, 9811) IEL, 'CM11', 'column element'
            CALL ERROR (FATAL, 3002, CPR, 0, 0, MSG)
            RETURN
         ENDIF
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
      IF (NCONCM.LT.1.OR.NCONCM.GT.NMAX (1) ) THEN
         WRITE (MSG, 9819) 'contaminants', 'CM19: NCONCM', NCONCM, NMAX (1)
         CALL ERROR (FATAL, 3003, CPR, 0, 0, MSG)
         RETURN
      ENDIF
      !
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! New code by SB for spatially distributed initial conditions
      ! -----------------------------------------------------------
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
            !           * Read the catagory type for each element into the element number
            CALL ALALLI (NUM_CATEGORIES_TYPES (I) , CMD, CPR, ':CM26c', NEL, NLF, NX, &
               NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, &
               NCATTY (NLF + 1, I) , IDUM)
            !
            !           * Table of values for each typical element
            DO 930 NC = 1, NUM_CATEGORIES_TYPES (I)
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
         IF (NREQ.GT.NELEE) THEN
            WRITE (MSG, 9809) NELEE, NREQ, 'flow-receiving columns', 'CM29: NFEX', NFEX
            CALL ERROR (FATAL, 3001, CPR, 0, 0, MSG)
            RETURN
         ENDIF
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
         IF (IEL.LE.NLF.OR.IEL.GT.NEL) THEN
            WRITE (MSG, 9811) IEL, 'CM31', 'column element'
            CALL ERROR (FATAL, 3002, CPR, 0, 0, MSG)
            RETURN
         ENDIF
         CALL DCOPY (NCONCM, DUMMY (INDX + 1), 1, CCAPE (IEL, 1), NELEE)
         INDX = INDX + 1 + NCONCM
312   END DO
      !
      !     * Default concentration at or convected into bases of columns
      CALL ALREDF (0, CMD, CPR, ':CM33', NCONCM, 1, DUMMY)
      DO 330 NCONT = 1, NCONCM
         IF (ISFLXB) THEN
            CALL ALINIT (DUMMY (NCONT), NEL - NLF, CCAPR (NLF + 1, NCONT) )
         ELSE
            CALL ALINIT (DUMMY (NCONT), NEL - NLF, CCAPB (NLF + 1, NCONT) )
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
         IF (NREQ.GT.NELEE) THEN
            WRITE (MSG, 9809) NELEE, NREQ, 'contaminant boundary column', 'CM37', NCBC
            CALL ERROR (FATAL, 3006, CPR, 0, 0, MSG)
            RETURN
         ENDIF
         CALL ALREDF (0, CMD, CPR, ':CM37', 1 + NCONCM, NCBC, DUMMY)
         INDX = 1
         DO 370 I = 1, NCBC
            IEL = NINT (DUMMY (INDX) )
            IF (IEL.LE.NLF.OR.IEL.GT.NEL) THEN
               WRITE (MSG, 9811) IEL, 'CM37', 'column element'
               CALL ERROR (FATAL, 3007, CPR, 0, 0, MSG)
               RETURN
            ENDIF
            IF (ISFLXB) THEN
               CALL DCOPY (NCONCM, DUMMY (INDX + 1), 1, CCAPR (IEL, 1), NELEE)
            ELSE
               CALL DCOPY (NCONCM, DUMMY (INDX + 1), 1, CCAPB (IEL, 1), NELEE)
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
      IF (NSCM.LT.1.OR.NSCM.GT.NMAX (2) ) THEN
         WRITE (MSG, 9819) 'soil types', 'CM21: NSCM', NSCM, NMAX (2)
         CALL ERROR (FATAL, 3004, CPR, 0, 0, MSG)
         RETURN
      ENDIF
      !
      !     * 3 size fractions (used only if SY module inactive)
      !     * (read soil index as extra column of floating-point data)
      CALL ALREDF (0, CMD, CPR, ':CM41', 4, NSCM, DUMMY)
      INDX = 1
      DO 410 I = 1, NSCM
         SOIL = NINT (DUMMY (INDX) )
         IF (SOIL.LT.1.OR.SOIL.GT.NSCM) THEN
            WRITE (MSG, 9811) SOIL, 'CM41', 'soil type'
            CALL ERROR (FATAL, 3008, CPR, 0, 0, MSG)
            RETURN
         ENDIF
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
      IF (NSEDCM.LT.1.OR.NSEDCM.GT.NMAX (3) ) THEN
         WRITE (MSG, 9819) 'sediment sizes', 'CM23: NSEDCM', NSEDCM, NMAX (3)
         CALL ERROR (FATAL, 3005, CPR, 0, 0, MSG)
         RETURN
      ENDIF
      !
      !     * Reference Kd for each particle size
      !     * (read contaminant index as extra column of floating-point data)
      CALL ALREDF (0, CMD, CPR, ':CM51', 1 + NSEDCM, NCONCM, DUMMY)
      INDX = 1
      DO 510 I = 1, NCONCM
         NCONT = NINT (DUMMY (INDX) )
         IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) THEN
            WRITE (MSG, 9811) NCONT, 'CM51', 'contaminant index'
            CALL ERROR (FATAL, 3011, CPR, 0, 0, MSG)
            RETURN
         ENDIF
         CALL DCOPY (NSEDCM, DUMMY (INDX + 1), 1, KDDLS (1, NCONT), 1)
         INDX = INDX + 1 + NSEDCM
510   END DO
      !
      !     * Coefficients for exchange between soil regions
      CALL ALREDF (0, CMD, CPR, ':CM53', 1 + NSCM, NCONCM, DUMMY)
      INDX = 1
      DO 530 I = 1, NCONCM
         NCONT = NINT (DUMMY (INDX) )
         IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) THEN
            WRITE (MSG, 9811) NCONT, 'CM53', 'contaminant index'
            CALL ERROR (FATAL, 3012, CPR, 0, 0, MSG)
            RETURN
         ENDIF
         CALL DCOPY (NSCM, DUMMY (INDX + 1), 1, ALPHA (1, NCONT), 1)
         INDX = INDX + 1 + NSCM
530   END DO
      !
      !     * Fraction of adsorption sites in dynamic region
      CALL ALREDF (0, CMD, CPR, ':CM55', 1 + NSCM, NCONCM, DUMMY)
      INDX = 1
      DO 550 I = 1, NCONCM
         NCONT = NINT (DUMMY (INDX) )
         IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) THEN
            WRITE (MSG, 9811) NCONT, 'CM55', 'contaminant index'
            CALL ERROR (FATAL, 3013, CPR, 0, 0, MSG)
            RETURN
         ENDIF
         CALL DCOPY (NSCM, DUMMY (INDX + 1), 1, FADS (1, NCONT), 1)
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
         IF (NCONT.LT.1.OR.NCONT.GT.NCONCM) THEN
            WRITE (MSG, 9811) NCONT, 'CM61', 'contaminant index'
            CALL ERROR (FATAL, 3014, CPR, 0, 0, MSG)
            RETURN
         ENDIF
         CALL DCOPY (NSCM, DUMMY (INDX + 1), 1, DISPDT (1, NCONT), 1)
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
      IF (NCONCM.LT.NCON.OR.NSCM.LT.NS.OR.NSEDCM.LT.NSED) THEN
         WRITE (MSG, 9800) NCONCM, NSCM, NSEDCM, NCON, NS, NSED
         CALL ERROR (FATAL, 3000, CPR, 0, 0, MSG)
         RETURN
      ENDIF
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
8230  WRITE (MSG, 9819) 'sediment sizes', 'CM23: NSEDCM', NSEDCM, NMAX (3)
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
8350  WRITE (MSG, 9809) NELEE, NREQ, 'non-default columns', 'CM35: NCBC', NCBC
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
      ! Formats
      ! -------
      !
9800  FORMAT ('No. of contaminants/soils/sediments with data' &
         ,' (CM19-23: NCONCM/NSCM/NSEDCM = ',2(I3,'/'),I3,')' &
         ,' must be at least ',2(I3,'/'),I3)
      !
      !     * length 91+2A
9809  FORMAT ('Insufficient workspace (have NELEE =',I6,', need',I6,')' &
         ,' for the number of ',A,' given (',A,' =',I6,')')
      !
      !     * length 57+2A
9811  FORMAT ('Index',I6,' (given as part of data item ',A,')' &
         ,' is not a valid ',A)
      !
      !     * length 73+2A
9819  FORMAT ('Number of ',A,' with data (',A,' =',I6,')' &
         ,' must be positive & not greater than',I6)

   END SUBROUTINE cm_read_data

END MODULE contaminant_data_reader
