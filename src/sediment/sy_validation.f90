!> summary: The `SYERR0`--`SYERR3` checks on the sediment input data.
!> author: AB / RAH / BTL, Newcastle University; JE, Newcastle University; Sven Berendsen
!>
!> Four routines that check the sediment input for range and consistency and
!> report every failure through [[error_reporting:RAISE_ERROR]]. They are
!> called from [[sy_driver:SYMAIN]] immediately after the data is read.
!>
!> These stay inside the component: the numbered diagnostics they issue are
!> sediment-specific, and nothing outside `sediment/` calls them.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-1995 | AB/RAH/BTL | 3.4.1 | Created sediment yield routines and later corrections, including `DLSMAX`. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the SY `.F` files into a single Fortran 90 module. |
!> | 2026-04 to 2026-05 | SvB | 4.6.1 | Modernised the whole component: free-form layout, `IMPLICIT NONE`/`INTENT` throughout, structured control flow in place of `GOTO`s, compile-time `PARAMETER`s for the cached first-call constants, and `symain`'s work arrays moved to allocate-once module storage. |
!> | 2026-09-10 | SvB | - | Split out of SYmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE sy_validation

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P, ione1, izero1, one1, zero1
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal, ERRLVL_error
   USE error_status, ONLY: errstat_alloc
   USE float_compare, ONLY: idimje
   USE input_validation, ONLY: ALCHK, ALCHKI
   USE linear_algebra, ONLY: dcopy
   USE sy_state, ONLY: face_outflow

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SYERR0, SYERR1, SYERR2, SYERR3

CONTAINS

!> Checks scalar dimensions and file units passed through the water-sediment interface.
!>
!> `SYERR0` is the first consistency guard for the sediment component. It
!> verifies that the static workspace dimensions supplied by the water model can
!> contain the sediment arrays before any `SY` input is read or state is
!> initialised.
!>
!> The checks require:
!>
!> | Quantity | Required relation |
!> |:---------|:------------------|
!> | `NELEE` | `>= max(NEL,NV,NX*NY)` |
!> | `NLFEE` | `>= max(1,NLF)` |
!> | `NLYREE`, `NSEDEE` | both `> 0` |
!> | `NSEE` | `>= NS` |
!> | `NVEE` | `>= NV` |
!> | `NXEE` | `>= NX` and `<= 9999` |
!> | `SPR`, `SYD` | both non-negative unit numbers |
!> | `NLF` | `0 <= NLF < NEL` |
!> | `NS`, `NV`, `NX`, `NY` | all `> 0` |
!>
!> Each failed relation is reported through `ALCHKI` on the sediment print unit
!> `SPR`. If any failures are found, the routine raises fatal error 2000 before
!> returning.
   SUBROUTINE SYERR0(NEL, NELEE, NLF, NLFEE, NLYREE, NS, NSEDEE, &
                     NSEE, NV, NVEE, NX, NXEE, NY, SPR, SYD)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: NEL   !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE !! Link-array dimension.
      INTEGER, INTENT(IN) :: NLYREE !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NS    !! Number of soil types.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      INTEGER, INTENT(IN) :: NSEE  !! Soil-type array dimension.
      INTEGER, INTENT(IN) :: NV    !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NVEE  !! Vegetation-array dimension.
      INTEGER, INTENT(IN) :: NX    !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NY    !! Number of grid rows.
      INTEGER, INTENT(IN) :: SPR   !! Sediment diagnostic output unit.
      INTEGER, INTENT(IN) :: SYD   !! Static sediment input unit.

      ! Modernization Fix: Added IZERO_ARR to replace the undeclared IZERO1
      ! and made IUNDEF a parameter to prevent passing uninitialized memory.
      INTEGER, PARAMETER :: IZERO_ARR(1) = [0]
      INTEGER, PARAMETER :: IUNDEF = 0

      INTEGER :: NERR, JEDUMDUM
      INTEGER :: IDUMS(1), IDUMO(1)
      LOGICAL :: LDUM1(1)

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------

      ! * Initialize local counters
      NERR = 0

      ! 1. Array Sizes
      ! --------------

      ! NELEE
      IDUMS(1) = NELEE
      IDUMO(1) = MAX(NEL, NV, NX*NY)
      CALL ALCHKI(ERRLVL_error, 2054, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! NLFEE
      IDUMS(1) = NLFEE
      IDUMO(1) = MAX(1, NLF)
      CALL ALCHKI(ERRLVL_error, 2055, SPR, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! NLYREE, NSEDEE
      IDUMS(1) = MIN(NLYREE, NSEDEE)
      CALL ALCHKI(ERRLVL_error, 2056, SPR, 1, 1, IUNDEF, IUNDEF, '[ NLYREE, NSEDEE ]', 'GT', IZERO_ARR, IDUMS, NERR, LDUM1)

      ! NSEE
      IDUMS(1) = NSEE
      IDUMO(1) = NS
      CALL ALCHKI(ERRLVL_error, 2057, SPR, 1, 1, IUNDEF, IUNDEF, 'NSEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! NVEE
      IDUMS(1) = NVEE
      IDUMO(1) = NV
      CALL ALCHKI(ERRLVL_error, 2058, SPR, 1, 1, IUNDEF, IUNDEF, 'NVEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! NXEE
      IDUMS(1) = NXEE
      IDUMO(1) = NX
      CALL ALCHKI(ERRLVL_error, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      IDUMO(1) = 9999
      CALL ALCHKI(ERRLVL_error, 2059, SPR, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'LE', IDUMO, IDUMS, NERR, LDUM1)

      ! 2. Unit Numbers
      ! ---------------

      ! SPR, SYD
      IDUMS(1) = MIN(SPR, SYD)
      CALL ALCHKI(ERRLVL_error, 2060, SPR, 1, 1, IUNDEF, IUNDEF, '[ SPR, SYD ]', 'GE', IZERO_ARR, IDUMS, NERR, LDUM1)

      ! 3. Number of Entities
      ! ---------------------

      ! NLF
      IDUMS(1) = NLF
      IDUMO(1) = NEL
      CALL ALCHKI(ERRLVL_error, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', IZERO_ARR, IDUMS, NERR, LDUM1)
      CALL ALCHKI(ERRLVL_error, 2061, SPR, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', IDUMO, IDUMS, NERR, LDUM1)

      ! NS, NV, NX, NY
      JEDUMDUM = MIN(NS, NV)
      IDUMS(1) = MIN(JEDUMDUM, NX, NY)
      CALL ALCHKI(ERRLVL_error, 2062, SPR, 1, 1, IUNDEF, IUNDEF, '[ NS, NV, NX, NY ]', 'GT', IZERO_ARR, IDUMS, NERR, LDUM1)

      ! 4. Epilogue
      ! -----------

      IF (NERR > 0) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 2000, SPR, 0, 0, 'Error(s) detected while checking WAT-SY interface variables')
      END IF

   END SUBROUTINE SYERR0

!> Checks static water-flow arrays required by the sediment component.
!>
!> `SYERR1` validates the static and initial water-model data that the sediment
!> routines rely on after [[syerr0]] has confirmed the workspace dimensions.
!> These checks protect the later erosion, routing, and bed-update routines from
!> invalid topology, out-of-range soil/vegetation indices, and non-physical
!> geometry.
!>
!> Main validation groups:
!>
!> | Group | Checks performed |
!> |:------|:-----------------|
!> | Element indexing | `ICMXY` and, when banks exist, `ICMBK` define exactly the expected column/bank elements with unique element identities. |
!> | Face adjacency | `ICMREF` neighbours are in range and regular element faces reflect back to the originating element and face. |
!> | Branch adjacency | `ICMRF2` branch references are unique, in range, and mirrored consistently through `ICMREF`. |
!> | Bank-neighbour topology | Each banked link has at least one neighbouring grid element when explicit banks are enabled. |
!> | Soil state | `THSAT(soil) <= 1`. |
!> | Channel geometry | `CLENTH >= 0`, `CWIDTH > 0`, `ZBFULL >= ZGRUND`, and `ARXL >= 0` for each link. |
!> | Column geometry/state | `DXQQ > 0`, `DYQQ > 0`, `HRF >= ZGRUND`, valid `NLYR`, valid top-layer soil type `NTSOIL(iel,NLYR)`, and valid vegetation type `NVC`. |
!> | Element geometry | `AREA > 0` and all face distances `DHF > 0`. |
!>
!> @note Bank-neighbour checks use the bank face normal to the link:
!> `FACE = 2*BANK`, decremented for north-south links, and require at least
!> one active grid neighbour across the two banks of each channel link.
!> @endnote
!>
!> Each failed relation is reported through `ALCHK` or `ALCHKI` on the sediment
!> print unit `SPR`. If any failures are found, the routine raises fatal error
!> 2001 before returning.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-05-03 | SvB | 4.6.1 | Replaced an uninitialised local `IUNDEF` "don't care" argument to `ALCHK`/`ALCHKI` with an explicit `PARAMETER = 0`. |
!> @endhistory
   SUBROUTINE SYERR1(NEL, NELEE, NLF, NLFEE, NLYREE, NS, NV, NX, &
                     NXEE, NYEE, NY, SPR, BEXBK, LINKNS, ICMBK, ICMXY, ICMREF, ICMRF2, NLYR, &
                     NTSOIL, NVC, THSAT, CLENTH, CWIDTH, ZBFULL, DXQQ, DYQQ, AREA, DHF, &
                     ARXL, HRF, ZGRUND, IDUM, IDUM1X, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: NEL    !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NLYREE !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NS     !! Number of soil types.
      INTEGER, INTENT(IN) :: NV     !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NX     !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE   !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NYEE   !! Grid-row workspace dimension.
      INTEGER, INTENT(IN) :: NY     !! Number of grid rows.
      INTEGER, INTENT(IN) :: SPR    !! Sediment diagnostic output unit.
      LOGICAL, INTENT(IN) :: BEXBK       !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE) !! True for north-south channel links.

      ! Read-Only Arrays (Used for reference or copied to scratchpads)
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)   !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)   !! Element number at each grid location.
      INTEGER, INTENT(IN) :: ICMRF2(NLFEE, 3, 2) !! Confluence branch reference map.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE) !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND(NEL) !! Ground or bed elevation by element.

      ! Arrays checked by ALCHK/ALCHKI (routines may use INTENT(INOUT) interfaces)
      INTEGER, INTENT(INOUT) :: ICMREF(NELEE, 4, 2:3) !! Face-neighbour and reverse-face reference map.
      INTEGER, INTENT(INOUT) :: NLYR(NLF + 1:NEL) !! Number of soil layers in each land element.
      INTEGER, INTENT(INOUT) :: NVC(NLF + 1:NEL)  !! Vegetation type by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: THSAT(NS)      !! Saturated water content by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: CLENTH(NLFEE)  !! Channel-link length.
      DOUBLE PRECISION, INTENT(INOUT) :: CWIDTH(NLFEE)  !! Channel width by link.
      DOUBLE PRECISION, INTENT(INOUT) :: ZBFULL(NLFEE)  !! Bankfull elevation/depth by link.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NLF + 1:NEL) !! Land-element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ(NLF + 1:NEL) !! Land-element length.
      DOUBLE PRECISION, INTENT(INOUT) :: AREA(NEL)      !! Element plan area.
      DOUBLE PRECISION, INTENT(INOUT) :: DHF(NELEE, 4)  !! Face-to-face hydraulic distance.
      DOUBLE PRECISION, INTENT(INOUT) :: ARXL(NLFEE)    !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(INOUT) :: HRF(NLF + 1:NEL) !! Land-element water level/head.

      ! Workspace arguments (INTENT(INOUT) as scratch space)
      INTEGER, INTENT(INOUT) :: IDUM(NXEE*NYEE) !! Integer workspace for identity checks.
      INTEGER, INTENT(INOUT) :: IDUM1X(-1:NEL + 1) !! Integer workspace for element identity checks.
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE) !! Logical workspace for element checks.

      ! Strict array/scalar parameters for shape matching in ALCHK
      INTEGER, PARAMETER          :: IZERO_ARR(1) = [0], IONE_ARR(1) = [1]
      DOUBLE PRECISION, PARAMETER :: ZERO_ARR(1) = [0.0D0], ONE_ARR(1) = [1.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0

      INTEGER :: BANK, COUNT, FACE, FADJ, FEL
      INTEGER :: IADJ, IBR, IBRADJ, ICOL1, IEL, IELP, ILYR, IX, IY
      INTEGER :: LINK, NCOL, NELP, NERR, P, PADJ
      INTEGER, PARAMETER :: IUNDEF = 0
      INTEGER :: IDUM1(2)
      LOGICAL :: BKXYOK, REFOK

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------
      NERR = 0
      ICOL1 = NLF + 1
      NELP = NEL + 1

      ! 1. Index Arrays
      ! ---------------

      ! ICMBK, ICMXY
      COUNT = NERR
      NCOL = 0

      DO IEL = 0, NLF
         IDUM1X(IEL) = 1
      END DO

      DO IEL = ICOL1, NELP
         IDUM1X(IEL) = 0
      END DO

      DO IY = 1, NY
         DO IX = 1, NX
            IEL = MAX(0, MIN(ICMXY(IX, IY), NELP))
            IDUM1X(IEL) = IDUM1X(IEL) + 1
            NCOL = NCOL + MIN(IEL, 1)
         END DO
      END DO

      IF (BEXBK .AND. NLF > 0) THEN
         NCOL = NCOL + 2*NLF
         DO BANK = 1, 2
            DO LINK = 1, NLF
               IEL = MAX(0, MIN(ICMBK(LINK, BANK), NELP))
               IDUM1X(IEL) = IDUM1X(IEL) + 1
            END DO
         END DO
      END IF

      IDUM1(1) = NEL - NLF
      IDUM1X(0) = NCOL

      CALL ALCHKI(ERRLVL_error, 2075, SPR, 1, 1, IUNDEF, IUNDEF, '#_column_elements', 'EQ', IDUM1, IDUM1X(0:), NERR, LDUM)
      CALL ALCHKI(ERRLVL_error, 2076, SPR, 1, NEL, IUNDEF, IUNDEF, 'element_count(iel)', 'EQ', IONE_ARR, IDUM1X(1:), NERR, LDUM)

      BKXYOK = COUNT == NERR

      ! ICMREF part 1
      IDUM1(1) = NEL
      IDUM1(2) = -NLFEE
      REFOK = .TRUE.

      DO FACE = 1, 4
         COUNT = NERR

      CALL ALCHKI(ERRLVL_error, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)', 'LE', IDUM1(1:1), ICMREF(1:, FACE, 2), NERR, LDUM)
      CALL ALCHKI(ERRLVL_error, 2077, SPR, 1, NEL, FACE, 2, 'ICMREF(iel,face,2)', 'GE', IDUM1(2:2), ICMREF(1:, FACE, 2), NERR, LDUM)

         IF (COUNT == NERR) THEN
            DO IEL = 1, NEL
               IADJ = ICMREF(IEL, FACE, 2)
               IF (IADJ <= 0) THEN
                  IDUM(IEL) = 0
               ELSE
                  FADJ = ICMREF(IEL, FACE, 3)
                  IF (FADJ < 1 .OR. FADJ > 4) THEN
                     IDUM(IEL) = 1
                  ELSE
                     IF (ICMREF(IADJ, FADJ, 2) /= IEL) THEN
                        IDUM(IEL) = 2
                     ELSE
                        IDUM(IEL) = 0
                        IF (ICMREF(IADJ, FADJ, 3) /= FACE) IDUM(IEL) = 3
                     END IF
                  END IF
               END IF
            END DO
         CALL ALCHKI(ERRLVL_error, 2078, SPR, 1, NEL, FACE, IUNDEF, 'status_of_ICMREF(iel,face)', 'EQ', IZERO_ARR, IDUM, NERR, LDUM)
         END IF
         REFOK = REFOK .AND. COUNT == NERR
      END DO

      ! ICMREF part 2 (bank element neighbours)
      IF (NLF > 0 .AND. BEXBK .AND. BKXYOK .AND. REFOK) THEN
         IDUM1X(-1) = -2
         IDUM1X(0) = 0

         DO IEL = 1, NEL
            IDUM1X(IEL) = -2
         END DO

         DO IY = 1, NY
            DO IX = 1, NX
               IEL = MAX(0, ICMXY(IX, IY))
               IDUM1X(IEL) = MIN(IEL, 1)
            END DO
         END DO

         DO LINK = 1, NLF
            IDUM(LINK) = 0
         END DO

         DO BANK = 1, 2
            DO LINK = 1, NLF
               IEL = ICMBK(LINK, BANK)
               FACE = 2*BANK
               IF (LINKNS(LINK)) FACE = FACE - 1
               IADJ = MAX(-1, ICMREF(IEL, FACE, 2))
               IDUM(LINK) = IDUM(LINK) + IDUM1X(IADJ)
            END DO
         END DO

 CALL ALCHKI(ERRLVL_error, 2079, SPR, 1, NLF, IUNDEF, IUNDEF, '#_grids_neighbouring_banks(link)', 'GT', IZERO_ARR, IDUM, NERR, LDUM)
      END IF

      ! ICMRF2
      IF (REFOK) THEN
         DO IBR = 1, NLFEE
            IDUM(IBR) = -1
         END DO

         DO FACE = 1, 4
            DO IEL = 1, NEL
               IADJ = ICMREF(IEL, FACE, 2)
               IF (IADJ < 0) THEN
                  IBR = -IADJ
                  IF (IDUM(IBR) >= 0) THEN
                     IDUM(IBR) = IDUM(IBR) + 1
                  ELSE
                     IDUM(IBR) = 0

                     DO P = 1, 3
                        IADJ = ICMRF2(IBR, P, 1)
                        IF (IADJ > NEL) THEN
                           IDUM(IBR) = IDUM(IBR) + P*10
                        ELSE IF (IADJ > 0) THEN
                           FADJ = ICMRF2(IBR, P, 2)
                           IF (FADJ < 1 .OR. FADJ > 4) THEN
                              IDUM(IBR) = IDUM(IBR) + P*100
                           ELSE
                              IBRADJ = -ICMREF(IADJ, FADJ, 2)
                              IF (IBRADJ < 1 .OR. IBRADJ > NLFEE) THEN
                                 IDUM(IBR) = IDUM(IBR) + P*1000
                              ELSE

                                 search_padj: DO PADJ = 1, 3
                                    IELP = ICMRF2(IBRADJ, PADJ, 1)
                                    IF (IELP == IEL) THEN
                                       FEL = ICMRF2(IBRADJ, PADJ, 2)
                                       IF (FEL == FACE) EXIT search_padj
                                    END IF
                                 END DO search_padj

                                 IF (PADJ > 3) IDUM(IBR) = IDUM(IBR) + P*10000

                              END IF
                           END IF
                        END IF
                     END DO
                  END IF
               END IF
            END DO
         END DO

       CALL ALCHKI(ERRLVL_error, 2080, SPR, 1, NLFEE, IUNDEF, IUNDEF, 'status_of_ICMRF2(branch)', 'LE', IZERO_ARR, IDUM, NERR, LDUM)
      END IF

      ! 2. Soil Properties
      ! ------------------
      CALL ALCHK(ERRLVL_error, 2063, SPR, 1, NS, IUNDEF, IUNDEF, 'THSAT(soil)', 'LE', ONE_ARR, ZERO_VAL, THSAT, NERR, LDUM)

      ! 3. Link Properties & Initial State
      ! ----------------------------------
      IF (NLF > 0) THEN
         CALL ALCHK(ERRLVL_error, 2064, SPR, 1, NLF, IUNDEF, IUNDEF, 'CLENTH(link)', 'GE', ZERO_ARR, ZERO_VAL, CLENTH, NERR, LDUM)
         CALL ALCHK(ERRLVL_error, 2065, SPR, 1, NLF, IUNDEF, IUNDEF, 'CWIDTH(link)', 'GT', ZERO_ARR, ZERO_VAL, CWIDTH, NERR, LDUM)
        CALL ALCHK(ERRLVL_error, 2066, SPR, 1, NLF, IUNDEF, IUNDEF, 'ZBFULL(link)', 'GEa', ZGRUND(1:), ZERO_VAL, ZBFULL, NERR, LDUM)
         CALL ALCHK(ERRLVL_error, 2067, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', 'GE', ZERO_ARR, ZERO_VAL, ARXL, NERR, LDUM)
      END IF

      ! 4. Column Properties & Initial State
      ! ------------------------------------
    CALL ALCHK(ERRLVL_error, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DXQQ(iel)', 'GT', ZERO_ARR, ZERO_VAL, DXQQ(ICOL1:), NERR, LDUM)
    CALL ALCHK(ERRLVL_error, 2068, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'DYQQ(iel)', 'GT', ZERO_ARR, ZERO_VAL, DYQQ(ICOL1:), NERR, LDUM)
      CALL ALCHK(ERRLVL_error, 2069, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'HRF(iel)', 'GEa', ZGRUND(ICOL1:), ZERO_VAL, HRF(ICOL1:), NERR, LDUM)

      COUNT = NERR
      IDUM1(1) = NLYREE
      CALL ALCHKI(ERRLVL_error, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'GT', IZERO_ARR, NLYR(ICOL1:), NERR, LDUM)
      CALL ALCHKI(ERRLVL_error, 2070, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NLYR(iel)', 'LE', IDUM1(1:1), NLYR(ICOL1:), NERR, LDUM)

      IF (COUNT == NERR) THEN
         DO IEL = ICOL1, NEL
            ILYR = NLYR(IEL)
            IDUM(IEL) = NTSOIL(IEL, ILYR)
         END DO
         IDUM1(1) = NS
CALL ALCHKI(ERRLVL_error, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NTSOIL[iel,NLYR(iel)]', 'GT', IZERO_ARR, IDUM(ICOL1:), NERR, LDUM)
         CALL ALCHKI(ERRLVL_error, 2071, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NTSOIL[iel,NLYR(iel)]', 'LE', IDUM1(1:1), IDUM(ICOL1:), NERR, LDUM)
      END IF

      COUNT = NERR
      IDUM1(1) = NV
      CALL ALCHKI(ERRLVL_error, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'GT', IZERO_ARR, NVC(ICOL1:), NERR, LDUM)
      CALL ALCHKI(ERRLVL_error, 2072, SPR, ICOL1, NEL, IUNDEF, IUNDEF, 'NVC(iel)', 'LE', IDUM1(1:1), NVC(ICOL1:), NERR, LDUM)

      ! 5. Element Properties
      ! ---------------------
      CALL ALCHK(ERRLVL_error, 2073, SPR, 1, NEL, IUNDEF, IUNDEF, 'AREA(iel)', 'GT', ZERO_ARR, ZERO_VAL, AREA, NERR, LDUM)
      DO FACE = 1, 4
     CALL ALCHK(ERRLVL_error, 2074, SPR, 1, NEL, FACE, IUNDEF, 'DHF(iel,face)', 'GT', ZERO_ARR, ZERO_VAL, DHF(1:, FACE), NERR, LDUM)
      END DO

      ! 6. Epilogue
      ! -----------
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 2001, SPR, 0, 0, 'Error(s) detected while checking static/initial WAT-SY interface')
      END IF

   END SUBROUTINE SYERR1

!> Checks sediment input arrays and category assignments.
!>
!> `SYERR2` validates the sediment data read from the manual's `SY11`-`SY64`
!> input groups after the water-sediment interface has been checked. It also
!> normalises several scalar values through the local `IDUM`/`DUMMY` work arrays
!> after `ALCHK`/`ALCHKI` have applied bounds.
!>
!> Main validation groups:
!>
!> | Group | Checks performed |
!> |:------|:-----------------|
!> | Control scalars | `NEPS >= 1`, `FPCRIT >= 0`, `DLSMAX >= 0`, and enough `NELEE` workspace for sediment work arrays. |
!> | Channel-only controls | When links exist, `0 <= ISUSED <= 1`, `0 <= NFINE <= min(1,NSED-1)`, `ALPHA >= 0` if fines exist, and `DCBEDO >= 0`. |
!> | Particle sizes | `DRSED(1) > 0` and subsequent representative diameters are non-decreasing. |
!> | Soil properties | `GKR >= 0`, `GKF >= 0`, `RHOSO > 0`, `BKB >= 0` for channel runs, and each `SOSDFN` row is non-negative and sums to 1 within tolerance. |
!> | Vegetation/drip properties | `XDRIP >= 0`, `DRDRIP > 0`, and `FDRIP >= 0`. |
!> | Link properties | Bank soil type `NTSOBK` is in `1:NS`, and bed porosity satisfies `0 <= PBSED < 1`. |
!> | Column properties | `FCROCK <= 1`, `FCG <= 1-FCROCK`, and loose-sediment porosity `0 <= PLS < 1`. |
!> | Initial state | `DLS >= 0`, all `FBETA >= 0` with each element summing to 1, and all mobile concentrations `FDEL >= 0`. |
!> | Boundary metadata | If sediment boundary records exist, category storage is large enough, boundary elements and faces are valid external faces, category numbers are in range, `GBC >= 0`, `ABC >= 0`, `BBC > 0`, and selected boundary file/unit references are non-negative. |
!>
!> The manual notes that sediment boundary-condition routines are not yet
!> implemented; this routine still validates `SY61`-`SY64` metadata and rating
!> coefficients so invalid input is caught consistently. If any failures are
!> found, fatal error 2000 is raised before returning.
!>
!> @note The workspace check labelled `NELEE` compares the required sediment
!> workspace with `NXEE*NYEE` stored in `IDUM`, not directly with the `NELEE`
!> argument. Later guarded sections still use `NELEE` to avoid overrunning
!> local arrays.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-05-04 | SvB | 4.6.1 | Changed the local `RDUM` workspace from an automatic array sized `NXEE*NYEE` to `ALLOCATABLE`, to reduce stack usage. |
!> | 2026-05-03 | SvB | 4.6.1 | Replaced an uninitialised local `IUNDEF` "don't care" argument to `ALCHK`/`ALCHKI` with an explicit `PARAMETER = 0`. |
!> | 2026-09-05 | SvB | - | Added STAT= and ERRMSG= reporting for all (de)allocations. |
!> @endhistory
   SUBROUTINE SYERR2(NXEE, NYEE, NEL, NELEE, NLF, NLFEE, NS, NSEE, NSED, NSEDEE, NV, NSYB, NSYBEE, &
                     NSYC, NSYCEE, SPR, ICMREF, ISUSED, NEPS, NFINE, SFB, SRB, ALPHA, DCBEDO, &
                     FPCRIT, DLSMAX, NTSOBK, NSYBCD, NBFACE, DRSED, BKB, GKF, GKR, RHOSO, SOSDFN, &
                     DRDRIP, FDRIP, XDRIP, PBSED, FCG, FCROCK, PLS, DLS, FBETA, FDEL, ABC, BBC, &
                     GBC, IDUM, DUMMY, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: NXEE   !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NYEE   !! Grid-row workspace dimension.
      INTEGER, INTENT(IN) :: NEL    !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF    !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NS     !! Number of soil types.
      INTEGER, INTENT(IN) :: NSEE   !! Soil-type array dimension.
      INTEGER, INTENT(IN) :: NSED   !! Number of sediment size classes.
      INTEGER, INTENT(IN) :: NSEDEE !! Sediment-size array dimension.
      INTEGER, INTENT(IN) :: NV     !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NSYB   !! Number of sediment boundary entries.
      INTEGER, INTENT(IN) :: NSYBEE !! Sediment-boundary array dimension.
      INTEGER, INTENT(IN) :: NSYC(4) !! Number of sediment boundary categories by boundary type.
      INTEGER, INTENT(IN) :: NSYCEE !! Sediment-boundary-category array dimension.
      INTEGER, INTENT(IN) :: SPR    !! Sediment diagnostic output unit.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2) !! Face-neighbour reference map.
      INTEGER, INTENT(IN) :: NBFACE(NEL) !! Number of boundary faces by element.
      INTEGER, INTENT(IN) :: SFB !! Sediment boundary file unit.
      INTEGER, INTENT(IN) :: SRB !! Sediment rating-boundary file unit.

      ! Input/Output arguments (Variables modified via ALCHK/ALCHKI checking/casting)
      INTEGER, INTENT(INOUT) :: ISUSED !! Sediment velocity option.
      INTEGER, INTENT(INOUT) :: NEPS   !! Number of sediment substeps per water timestep.
      INTEGER, INTENT(INOUT) :: NFINE  !! Number of fine sediment classes.
      INTEGER, INTENT(INOUT) :: NTSOBK(NLFEE)     !! Bank soil type by link.
      INTEGER, INTENT(INOUT) :: NSYBCD(NSYBEE, 3) !! Sediment boundary element, type, and category metadata.
      DOUBLE PRECISION, INTENT(INOUT) :: ALPHA  !! Fine-sediment settling/resuspension critical-shear ratio.
      DOUBLE PRECISION, INTENT(INOUT) :: DCBEDO !! Active upper channel-bed layer thickness.
      DOUBLE PRECISION, INTENT(INOUT) :: FPCRIT !! Maximum sediment concentration fraction.
      DOUBLE PRECISION, INTENT(INOUT) :: DLSMAX !! Loose-sediment depth above which hillslope soil erosion is suppressed.
      DOUBLE PRECISION, INTENT(INOUT) :: DRSED(NSED) !! Representative sediment particle diameters.
      DOUBLE PRECISION, INTENT(INOUT) :: BKB(NS)   !! Bank erodibility by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: GKF(NS)   !! Flow detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: GKR(NS)   !! Rainfall detachment coefficient by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: RHOSO(NS) !! Soil bulk density by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: SOSDFN(NSEE, NSED) !! Soil sediment-size fractions by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: DRDRIP(NV) !! Canopy drip drop diameter by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: FDRIP(NV)  !! Canopy drip fraction by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: XDRIP(NV)  !! Canopy drip fall height by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: PBSED(NLFEE) !! Channel-bed sediment porosity by link.
      DOUBLE PRECISION, INTENT(INOUT) :: FCG(NLF + 1:NEL)    !! Ground-cover fraction by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: FCROCK(NLF + 1:NEL) !! Rock-cover fraction by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: PLS(NLF + 1:NEL)    !! Loose-sediment porosity by land element.
      DOUBLE PRECISION, INTENT(INOUT) :: DLS(NEL) !! Loose/bed sediment depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: FBETA(NELEE, NSED) !! Sediment composition fraction by element and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: FDEL(NELEE, NSED)  !! Mobile sediment concentration fraction by element and size class.
      DOUBLE PRECISION, INTENT(INOUT) :: ABC(NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `A` by sediment class/category.
      DOUBLE PRECISION, INTENT(INOUT) :: BBC(NSEDEE, NSYCEE) !! Boundary rating-curve coefficient `B` by sediment class/category.
      DOUBLE PRECISION, INTENT(INOUT) :: GBC(NSEDEE, NSYCEE) !! Steady boundary sediment input by class/category.

      ! Workspace arguments
      INTEGER, DIMENSION(NXEE*NYEE), INTENT(INOUT)      :: IDUM  !! Integer workspace for grid/category checks.
      DOUBLE PRECISION, DIMENSION(NELEE), INTENT(INOUT) :: DUMMY !! Floating-point workspace for element checks.
      LOGICAL, DIMENSION(NELEE), INTENT(INOUT)          :: LDUM  !! Logical workspace for element checks.

      DOUBLE PRECISION, PARAMETER :: TOL = 1.0D-10

      INTEGER :: BB, COUNT, FACE, ICAT, IEL, ITYPE, NERR
      INTEGER, PARAMETER :: IUNDEF = 0
      INTEGER :: SED, SOIL, jedumdum
      INTEGER :: IDUM1(1)
      DOUBLE PRECISION, ALLOCATABLE :: RDUM(:)

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.

      !----------------------------------------------------------------------*

      ! 0. Preliminaries
      ! ----------------
      !     * Local counter
      ALLOCATE (RDUM(NXEE*NYEE), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "RDUM", "sy_validation:SYERR2", emsg)

      NERR = 0

      ! 1. Static Variables
      ! -------------------

      ! NEPS
      IDUM(1) = NEPS
      CALL ALCHKI(ERRLVL_error, 2012, SPR, 1, 1, IUNDEF, IUNDEF, 'NEPS', 'GE', IONE1, IDUM, NERR, LDUM)
      NEPS = IDUM(1)

      ! FPCRIT
      DUMMY(1) = FPCRIT
      CALL ALCHK(ERRLVL_error, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'FPCRIT', 'GE', ZERO1, ZERO1(1), DUMMY, NERR, LDUM)
      FPCRIT = DUMMY(1)

      ! DLSMAX
      DUMMY(1) = DLSMAX
      CALL ALCHK(ERRLVL_error, 2013, SPR, 1, 1, IUNDEF, IUNDEF, 'DLSMAX', 'GE', ZERO1, ZERO1(1), DUMMY, NERR, LDUM)
      DLSMAX = DUMMY(1)

      IF (NLF > 0) THEN
         ! ISUSED
         IDUM(1) = ISUSED
         CALL ALCHKI(ERRLVL_error, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', 'GE', IZERO1, IDUM, NERR, LDUM)
         CALL ALCHKI(ERRLVL_error, 2014, SPR, 1, 1, IUNDEF, IUNDEF, 'ISUSED', 'LE', IONE1, IDUM, NERR, LDUM)
         ISUSED = IDUM(1)

         ! NFINE
         IDUM(1) = NFINE
         IDUM1(1) = MIN(1, NSED - 1)
         CALL ALCHKI(ERRLVL_error, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', 'GE', IZERO1, IDUM, NERR, LDUM)
         CALL ALCHKI(ERRLVL_error, 2015, SPR, 1, 1, IUNDEF, IUNDEF, 'NFINE', 'LE', IDUM1, IDUM, NERR, LDUM)
         NFINE = IDUM(1)

         ! ALPHA
         IF (NFINE > 0) THEN
            DUMMY(1) = ALPHA
            CALL ALCHK(ERRLVL_error, 2016, SPR, 1, 1, IUNDEF, IUNDEF, 'ALPHA', 'GE', ZERO1, ZERO1(1), DUMMY, NERR, LDUM)
            ALPHA = DUMMY(1)
         END IF

         ! DCBEDO
         DUMMY(1) = DCBEDO
         CALL ALCHK(ERRLVL_error, 2017, SPR, 1, 1, IUNDEF, IUNDEF, 'DCBEDO', 'GE', ZERO1, ZERO1(1), DUMMY, NERR, LDUM)
         DCBEDO = DUMMY(1)
      END IF

      ! NELEE
      IDUM(1) = NXEE*NYEE
      jedumdum = IDIMJE(NSED, NFINE)
      jedumdum = jedumdum*NLF
      IDUM1(1) = MAX(NSED, jedumdum)

      ! * (including local workspace requirements)
      IDUM1(1) = MAX(IDUM1(1), NS, NSYB*2)
      CALL ALCHKI(ERRLVL_error, 2018, SPR, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUM1, IDUM, NERR, LDUM)

      ! 2. Sediment, Soil & Vegetation Properties
      ! -----------------------------------------
      !
      ! * Not enough workspace? (Converted GOTO 300 to block IF)
      IF (NELEE >= MAX(NSED, NS)) THEN

         ! DRSED
         COUNT = NERR
         CALL ALCHK(ERRLVL_error, 2019, SPR, 1, 1, IUNDEF, IUNDEF, 'DRSED(sed)', 'GT', ZERO1, ZERO1(1), DRSED(1), NERR, LDUM)

         IF (NSED > 1 .AND. NERR == COUNT) THEN
            CALL DCOPY(NSED - 1, DRSED, 1, RDUM, 1)
            IDUM(1:NSED - 1) = INT(RDUM(1:NSED - 1))
            CALL ALCHK(ERRLVL_error, 2019, SPR, 2, NSED, IUNDEF, IUNDEF, 'DRSED(sed)', 'GEa', RDUM, ZERO1(1), DRSED(2), NERR, LDUM)
         END IF

         ! GKR
         CALL ALCHK(ERRLVL_error, 2020, SPR, 1, NS, IUNDEF, IUNDEF, 'GKR(soil)', 'GE', ZERO1, ZERO1(1), GKR, NERR, LDUM)
         ! GKF
         CALL ALCHK(ERRLVL_error, 2021, SPR, 1, NS, IUNDEF, IUNDEF, 'GKF(soil)', 'GE', ZERO1, ZERO1(1), GKF, NERR, LDUM)
         ! RHOSO
         CALL ALCHK(ERRLVL_error, 2022, SPR, 1, NS, IUNDEF, IUNDEF, 'RHOSO(soil)', 'GT', ZERO1, ZERO1(1), RHOSO, NERR, LDUM)

         ! BKB
         IF (NLF > 0) THEN
            CALL ALCHK(ERRLVL_error, 2023, SPR, 1, NS, IUNDEF, IUNDEF, 'BKB(soil)', 'GE', ZERO1, ZERO1(1), BKB, NERR, LDUM)
         END IF

         ! SOSDFN
         DUMMY(1:NS) = ZERO1(1)
         DO SED = 1, NSED
            DO SOIL = 1, NS
               DUMMY(SOIL) = DUMMY(SOIL) + SOSDFN(SOIL, SED)
            END DO
      CALL ALCHK(ERRLVL_error, 2024, SPR, 1, NS, SED, IUNDEF, 'SOSDFN(soil,sed)', 'GE', ZERO1, ZERO1(1), SOSDFN(1, SED), NERR, LDUM)
         END DO
     CALL ALCHK(ERRLVL_error, 2024, SPR, 1, NS, IUNDEF, IUNDEF, 'SOSDFN[*][sum_over_sed](soil)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)

         ! XDRIP
         CALL ALCHK(ERRLVL_error, 2025, SPR, 1, NV, IUNDEF, IUNDEF, 'XDRIP(veg)', 'GE', ZERO1, ZERO1(1), XDRIP, NERR, LDUM)
         ! DRDRIP
         CALL ALCHK(ERRLVL_error, 2026, SPR, 1, NV, IUNDEF, IUNDEF, 'DRDRIP(veg)', 'GT', ZERO1, ZERO1(1), DRDRIP, NERR, LDUM)
         ! FDRIP
         CALL ALCHK(ERRLVL_error, 2027, SPR, 1, NV, IUNDEF, IUNDEF, 'FDRIP(veg)', 'GE', ZERO1, ZERO1(1), FDRIP, NERR, LDUM)

      END IF

      ! 3. Link Element Properties
      ! --------------------------
      !
      IF (NLF > 0) THEN
         ! NTSOBK
         IDUM(1) = NS
         CALL ALCHKI(ERRLVL_error, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'GE', IONE1, NTSOBK, NERR, LDUM)
         CALL ALCHKI(ERRLVL_error, 2028, SPR, 1, NLF, IUNDEF, IUNDEF, 'NTSOBK(link)', 'LE', IDUM, NTSOBK, NERR, LDUM)
         ! PBSED
         CALL ALCHK(ERRLVL_error, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', 'GE', ZERO1, ZERO1(1), PBSED, NERR, LDUM)
         CALL ALCHK(ERRLVL_error, 2029, SPR, 1, NLF, IUNDEF, IUNDEF, 'PBSED(link)', 'LT', ONE1, ZERO1(1), PBSED, NERR, LDUM)
      END IF

      ! 4. Column-element Properties
      ! ----------------------------
      !
      ! FCROCK
      CALL ALCHK(ERRLVL_error, 2030, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCROCK(iel)', 'LE', ONE1, ZERO1(1), FCROCK, NERR, LDUM)

      ! FCG
      DO IEL = NLF + 1, NEL
         DUMMY(IEL) = ONE1(1) - FCROCK(IEL)
      END DO
     CALL ALCHK(ERRLVL_error, 2031, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'FCG(iel)', 'LEa', DUMMY(NLF + 1), ZERO1(1), FCG, NERR, LDUM)

      ! PLS
      CALL ALCHK(ERRLVL_error, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'GE', ZERO1, ZERO1(1), PLS, NERR, LDUM)
      CALL ALCHK(ERRLVL_error, 2032, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'PLS(iel)', 'LT', ONE1, ZERO1(1), PLS, NERR, LDUM)

      ! 5. All-element Initialization
      ! -----------------------------
      !
      ! DLS
      CALL ALCHK(ERRLVL_error, 2033, SPR, 1, NEL, IUNDEF, IUNDEF, 'DLS(iel)', 'GE', ZERO1, ZERO1(1), DLS, NERR, LDUM)

      ! FBETA
      DUMMY(1:NEL) = ZERO1(1)
      DO SED = 1, NSED
         DO IEL = 1, NEL
            DUMMY(IEL) = DUMMY(IEL) + FBETA(IEL, SED)
         END DO
        CALL ALCHK(ERRLVL_error, 2034, SPR, 1, NEL, SED, IUNDEF, 'FBETA(iel,sed)', 'GE', ZERO1, ZERO1(1), FBETA(1, SED), NERR, LDUM)
      END DO
      CALL ALCHK(ERRLVL_error, 2034, SPR, 1, NEL, IUNDEF, IUNDEF, 'FBETA[*][sum_over_sed](iel)', 'EQ', ONE1, TOL, DUMMY, NERR, LDUM)

      ! FDEL
      DO SED = 1, NSED
         CALL ALCHK(ERRLVL_error, 2035, SPR, 1, NEL, SED, IUNDEF, 'FDEL(iel,sed)', 'GE', ZERO1, ZERO1(1), FDEL(1, SED), NERR, LDUM)
      END DO

      ! 6. Boundary Data
      ! ----------------
      !
      IF (NSYB > 0) THEN
         IF (NELEE >= NSYB*2) THEN

            ! NSYCEE
            IDUM(1) = NSYCEE
            IDUM1(1) = MAX(NSYC(1) + NSYC(2), NSYC(3) + NSYC(4))
            CALL ALCHKI(ERRLVL_error, 2036, SPR, 1, 1, IUNDEF, IUNDEF, 'NSYCEE', 'GE', IDUM1, IDUM, NERR, LDUM)

            ! NSYBCD(BB,1)
            COUNT = NERR
            IDUM1(1) = NEL
            CALL ALCHKI(ERRLVL_error, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', 'GE', IONE1, NSYBCD, NERR, LDUM)
            CALL ALCHKI(ERRLVL_error, 2037, SPR, 1, NSYB, 1, IUNDEF, 'NSYBCD(bdry,1)', 'LE', IDUM1, NSYBCD, NERR, LDUM)

            ! NBFACE
            IF (COUNT == NERR) THEN
               DO BB = 1, NSYB
                  IEL = NSYBCD(BB, 1)
                  IDUM(BB) = NBFACE(IEL)
               END DO
               IDUM1(1) = 4
          CALL ALCHKI(ERRLVL_error, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, 'NBFACE[NSYBCD[*][1]](bdry)', 'GE', IONE1, IDUM, NERR, LDUM)
          CALL ALCHKI(ERRLVL_error, 2038, SPR, 1, NSYB, IUNDEF, IUNDEF, 'NBFACE[NSYBCD[*][1]](bdry)', 'LE', IDUM1, IDUM, NERR, LDUM)
            END IF

            ! ICMREF
            IF (COUNT == NERR) THEN
               DO BB = 1, NSYB
                  IEL = NSYBCD(BB, 1)
                  FACE = NBFACE(IEL)
                  IDUM(BB) = ICMREF(IEL, FACE, 2)
               END DO
               CALL ALCHKI (ERRLVL_error, 2039, SPR, 1, NSYB, IUNDEF, IUNDEF, 'ICMREF[NSYBCD[*][1]][NBFACE][2](bdry)', 'EQ', IZERO1, IDUM, NERR, LDUM)
            END IF

            ! NSYBCD(BB,3)
            DO BB = 1, NSYB
               ITYPE = NSYBCD(BB, 2)
               IDUM(BB) = 1
               IF (MOD(ITYPE, 2) == 0) IDUM(BB) = IDUM(BB) + NSYC(ITYPE - 1)
               IDUM(NSYB + BB) = IDUM(BB) + NSYC(ITYPE)
            END DO
            CALL ALCHKI(ERRLVL_error, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', 'GE', IDUM, NSYBCD(1, 3), NERR, LDUM)
          CALL ALCHKI(ERRLVL_error, 2040, SPR, 1, NSYB, 3, IUNDEF, 'NSYBCD(bdry,3)', 'LE', IDUM(NSYB + 1), NSYBCD(1, 3), NERR, LDUM)

            ! GBC
            DO ICAT = 1, NSYC(1)
        CALL ALCHK(ERRLVL_error, 2041, SPR, 1, NSED, ICAT, IUNDEF, 'GBC(sed,icat)', 'GE', ZERO1, ZERO1(1), GBC(1, ICAT), NERR, LDUM)
            END DO

            ! ABC
            DO ICAT = 1, NSYC(3)
        CALL ALCHK(ERRLVL_error, 2042, SPR, 1, NSED, ICAT, IUNDEF, 'ABC(sed,icat)', 'GE', ZERO1, ZERO1(1), ABC(1, ICAT), NERR, LDUM)
            END DO

            ! BBC
            DO ICAT = 1, NSYC(3)
        CALL ALCHK(ERRLVL_error, 2043, SPR, 1, NSED, ICAT, IUNDEF, 'BBC(sed,icat)', 'GT', ZERO1, ZERO1(1), BBC(1, ICAT), NERR, LDUM)
            END DO

            ! SFB
            IF (NSYC(2) > 0) THEN
               IDUM(1) = SFB
               CALL ALCHKI(ERRLVL_error, 2044, SPR, 1, 1, IUNDEF, IUNDEF, 'SFB', 'GE', IZERO1, IDUM, NERR, LDUM)
            END IF

            ! SRB
            IF (NSYC(2) > 0) THEN
               IDUM(1) = SRB
               CALL ALCHKI(ERRLVL_error, 2045, SPR, 1, 1, IUNDEF, IUNDEF, 'SRB', 'GE', IZERO1, IDUM, NERR, LDUM)
            END IF
         END IF
      END IF

      ! 7. Epilogue
      ! -----------
      !
      IF (NERR > 0) CALL RAISE_ERROR(ERRLVL_fatal, 2000, SPR, 0, 0, 'Error(s) detected while checking SY input data')

   END SUBROUTINE SYERR2

!> Checks time-dependent water-flow values before a sediment timestep.
!>
!> `SYERR3` validates the current water-model state passed to the sediment
!> component. It is called at runtime, after the static interface checks, to
!> catch non-physical transient values and routing-order inconsistencies before
!> erosion and sediment advection use them.
!>
!> Main validation groups:
!>
!> | Group | Checks performed |
!> |:------|:-----------------|
!> | Time step | `DTUZ >= 0`. |
!> | Vegetation state | `CLAI >= 0` and `0 <= PLAI <= 1`. |
!> | Link state | `ARXL >= 0` for active links. |
!> | Column water inputs | `DRAINA >= 0` and `DRAINA <= PNETTO` within tolerance. |
!> | Element water level | `HRF >= ZGRUND`. |
!> | Flow consistency | Adjacent regular faces must not both discharge into each other (`status=1`), branch outflows must have a receiving neighbour (`status=2`), and donor elements must precede receptors in `ISORT`. |
!>
!> Face outflow is interpreted as
!>
!> \[
!>   Q_{out}(iel,face) = \operatorname{sign}(1,2-face)\,QOC(iel,face).
!> \]
!>
!> With the implemented Fortran `SIGN` rule, positive `QOC` is outflow on faces
!> 1 and 2, while negative `QOC` is outflow on faces 3 and 4.
!>
!> The routine builds `JSORT`, the inverse of `ISORT`, and `JMIN`, the earliest
!> receptor position required by each donor. Any flow-order failure is reported
!> with `ALCHKI`. If errors are found, the relevant transient arrays are written
!> to `SPR`, then error 2003 is raised before returning.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-04-06 | SvB | 4.6.1 | Replaced the `GOTO 640` non-discharge-face skip with `CYCLE element_loop`, and the legacy statement function used to evaluate face outflow with the internal `FUNCTION` `FNQOUT`. |
!> | 2026-09-20 | SvB | - | Replaced `FNQOUT` with the shared [[sy_state:face_outflow]], which [[sywat]] duplicated as `FQOUT`. |
!> | 2026-05-03 | SvB | 4.6.1 | Replaced an uninitialised local `IUNDEF` "don't care" argument to `ALCHK`/`ALCHKI` with an explicit `PARAMETER = 0`. |
!> @endhistory
   SUBROUTINE SYERR3(NEL, NELEE, NLF, NLFEE, NV, SPR, ICMREF, &
                     ICMRF2, ISORT, DTUZ, CLAI, PLAI, ARXL, DRAINA, PNETTO, HRF, &
                     ZGRUND, QOC, IQ, JMIN, JSORT, LDUM)

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN) :: NEL   !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF   !! Number of channel links.
      INTEGER, INTENT(IN) :: NLFEE !! Link-array dimension.
      INTEGER, INTENT(IN) :: NV    !! Number of vegetation types.
      INTEGER, INTENT(IN) :: SPR   !! Sediment diagnostic output unit.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:3) !! Face-neighbour and reverse-face reference map.
      INTEGER, INTENT(IN) :: ICMRF2(NLFEE, 3, 2)   !! Confluence branch reference map.
      INTEGER, INTENT(IN) :: ISORT(NEL) !! Donor-before-receptor element routing order.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(INOUT) :: CLAI(NV)    !! Current canopy leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: PLAI(NV)    !! Potential/maximum leaf-area index by vegetation type.
      DOUBLE PRECISION, INTENT(INOUT) :: ARXL(NLFEE) !! Channel cross-sectional area by link.
      DOUBLE PRECISION, INTENT(INOUT) :: DRAINA(NLF + 1:NEL) !! Canopy-drip rainfall reaching the ground.
      DOUBLE PRECISION, INTENT(INOUT) :: HRF(NEL)    !! Water level/head by element.
      DOUBLE PRECISION, INTENT(IN) :: PNETTO(NLF + 1:NEL) !! Net precipitation/effective rainfall by land element.
      DOUBLE PRECISION, INTENT(IN) :: ZGRUND(NEL) !! Ground or bed elevation by element.
      DOUBLE PRECISION, INTENT(IN) :: QOC(NELEE, 4) !! Face water fluxes.

      ! Workspace arguments
      INTEGER :: IQ(NEL)    !! Per-face donor/receptor flow-consistency status by element.
      INTEGER :: JMIN(NEL)  !! Earliest required `ISORT` position for each element's receptors.
      INTEGER :: JSORT(0:NEL + 1) !! Inverse of `ISORT`: position of each element in the routing order.
      LOGICAL :: LDUM(NELEE) !! Logical workspace for `ALCHK`/`ALCHKI` checks.

      DOUBLE PRECISION, PARAMETER :: TOL = 1.0D-7
      !
      INTEGER :: FACE, FADJ, I, IADJ, IBR, IEL, J, NELP, NERR, P
      INTEGER, PARAMETER :: IUNDEF = 0
      DOUBLE PRECISION :: QADJ, QMIN
      DOUBLE PRECISION :: DUM1(1)

      !----------------------------------------------------------------------*
      !
      ! 0. Preliminaries
      ! ----------------
      !
      !     * Initialize local counter
      NERR = 0
      !
      !
      ! 1. Variables
      ! ------------
      !
      ! DTUZ
      DUM1(1) = DTUZ
      CALL ALCHK(ERRLVL_error, 2046, SPR, 1, 1, IUNDEF, IUNDEF, 'DTUZ', 'GE', &
                 zero1, zero1(1), DUM1, NERR, LDUM)
      !
      !
      ! 2. Vegetative State
      ! -------------------
      !
      ! CLAI
      CALL ALCHK(ERRLVL_error, 2047, SPR, 1, NV, IUNDEF, IUNDEF, 'CLAI(veg)', &
                 'GE', zero1, zero1(1), CLAI, NERR, LDUM)
      ! PLAI
      CALL ALCHK(ERRLVL_error, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
                 'GE', zero1, zero1(1), PLAI, NERR, LDUM)
      CALL ALCHK(ERRLVL_error, 2048, SPR, 1, NV, IUNDEF, IUNDEF, 'PLAI(veg)', &
                 'LE', ONE1, ZERO1(1), PLAI, NERR, LDUM)
      !
      !
      ! 3. Link State
      ! -------------
      !
      IF (NLF > 0) THEN
         !
         ! ARXL
         CALL ALCHK(ERRLVL_error, 2049, SPR, 1, NLF, IUNDEF, IUNDEF, 'ARXL(link)', &
                    'GE', zero1, zero1(1), ARXL, NERR, LDUM)
         !
      END IF
      !
      !
      ! 4. Columnar State
      ! -----------------
      !
      ! DRAINA
      CALL ALCHK(ERRLVL_error, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'GE', zero1, zero1(1), DRAINA, NERR, LDUM)
      ! 10.10.94  Ought to fix WAT module so that we don't need TOL
      CALL ALCHK(ERRLVL_error, 2050, SPR, NLF + 1, NEL, IUNDEF, IUNDEF, 'DRAINA(iel)', 'LEa', PNETTO, TOL, DRAINA, NERR, LDUM)
      !
      !
      ! 5. Elemental State
      ! ------------------
      !
      ! HRF
      CALL ALCHK(ERRLVL_error, 2051, SPR, 1, NEL, IUNDEF, IUNDEF, 'HRF(iel)', &
                 'GEa', ZGRUND, ZERO1(1), HRF, NERR, LDUM)
      !
      !
      ! 6. Flux/Ordering
      ! ----------------
      !
      ! ISORT & QOC
      !     * Set JSORT = inverse of ISORT & initialize upper bound JMIN
      !       (note that JSORT has overspill elements )
      NELP = NEL + 1
      DO J = 0, NELP
         JSORT(J) = NELP
      END DO

      DO I = 1, NEL
         IEL = ISORT(I)
         J = MAX(0, MIN(IEL, NELP))
         JSORT(J) = I
         JMIN(I) = NELP
      END DO

      !     * At this point any element not listed in ISORT has a JSORT
      !       value of NELP, which is guaranteed to fail the test below
      !     * Update JMIN (used as object of JSORT test) & set QOC status IQ
      DO FACE = 1, 4

         element_loop: DO IEL = 1, NEL
            !          * innocent until proven guilty
            IQ(IEL) = 0

            !          * non-discharge faces are ok (Cycle directly replaces GOTO 640)
            IF (face_outflow(QOC, IEL, FACE) <= ZERO1(1)) CYCLE element_loop

            IADJ = ICMREF(IEL, FACE, 2)

            IF (IADJ > 0) THEN
               FADJ = ICMREF(IEL, FACE, 3)
               QADJ = face_outflow(QOC, IADJ, FADJ)
               !             * do both elements discharge into the same face?
               IF (QADJ > ZERO1(1)) IQ(IEL) = 1
               !             * IEL must precede IADJ in the ISORT list
               JMIN(IEL) = MIN(JSORT(IADJ), JMIN(IEL))

            ELSE IF (IADJ < 0) THEN
               IBR = -IADJ
               QMIN = ONE1(1)

               DO P = 1, 3
                  IADJ = ICMRF2(IBR, P, 1)
                  IF (IADJ > 0) THEN
                     FADJ = ICMRF2(IBR, P, 2)
                     QADJ = face_outflow(QOC, IADJ, FADJ)
                     QMIN = MIN(QADJ, QMIN)
                     IF (QADJ < zero1(1)) THEN
                        !                      * IEL must precede IADJ in the ISORT list
                        JMIN(IEL) = MIN(JSORT(IADJ), JMIN(IEL))
                     END IF
                  END IF
               END DO

               !             * discharge from IEL has nowhere to go?
               IF (QMIN >= zero1(1)) IQ(IEL) = 2
            END IF
         END DO element_loop

         !        * Check QOC status at this FACE for all elements
         CALL ALCHKI(ERRLVL_error, 2052, SPR, 1, NEL, FACE, IUNDEF, &
                     'status_of_QOC(iel,face)', 'EQ', IZERO1, IQ, NERR, LDUM)

      END DO

      !     * Check that each donor element listed in ISORT occurs before
      !       each of its receptors, and that all elements are listed
      CALL ALCHKI(ERRLVL_error, 2053, SPR, 1, NEL, IUNDEF, IUNDEF, &
                  'position_in_ISORT(iel)', 'LTa', JMIN, JSORT(1), NERR, LDUM)
      !
      !
      ! 7. Epilogue
      ! -----------
      !
      IF (NERR > 0) THEN
         !
         WRITE (SPR, 9100) 'DTUZ', DTUZ
         WRITE (SPR, 9100) 'CLAI[veg=1,...,NV]', CLAI
         WRITE (SPR, 9100) 'PLAI[veg=1,...,NV]', PLAI
         WRITE (SPR, 9100) 'ARXL[link=1,...,NLF]', (ARXL(IEL), IEL=1, NLF)
         WRITE (SPR, 9100) 'DRAINA[col=NLF+1,...,NEL]', DRAINA
         WRITE (SPR, 9100) 'PNETTO[col=NLF+1,...,NEL]', PNETTO
         WRITE (SPR, 9100) 'HRF[iel=1,...,NEL]', (HRF(IEL), IEL=1, NEL)
         WRITE (SPR, 9100) 'ZGRUND[iel=1,...,NEL]', ZGRUND
         WRITE (SPR, 9200) 'ISORT[iel=1,...,NEL]', ISORT
         WRITE (SPR, 9200) 'position_in_ISORT[iel=1,...,NEL]', (JSORT(IEL), IEL=1, NEL)

         DO FACE = 1, 4
            WRITE (SPR, 9150) 'QOC[iel=1,...,NEL][face=', FACE, ']', (QOC(IEL, FACE), IEL=1, NEL)
         END DO
         !
         CALL RAISE_ERROR(ERRLVL_error, 2003, SPR, 0, 0, 'Error(s) detected while checking time-dependent WAT-SY interface')
         !
      END IF

      RETURN

      ! FORMAT STATEMENTS safely at the bottom
9100  FORMAT(1X, A, ':'/1P, (8E10.2))
9150  FORMAT(1X, A, I1, A, ':'/1P, (8E10.2))
9200  FORMAT(1X, A, ':'/(16I5))

   END SUBROUTINE SYERR3

END MODULE sy_validation

