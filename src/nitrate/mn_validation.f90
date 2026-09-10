!> summary: The `MNERR0`--`MNERR4` checks on the nitrate input data.
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> Five routines that check the nitrate input for range and consistency and
!> report every failure through [[error_reporting:RAISE_ERROR]]. They are
!> called from [[mn_driver:MNINITIALISE]] immediately after the data is read,
!> so a bad record is reported against the record rather than as a later
!> modelling failure.
!>
!> These stay inside the component: the numbered diagnostics they issue are
!> nitrate-specific, and nothing outside `nitrate/` calls them.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-03 | Stephen Birkinshaw | 4.6 | Added the current nitrate component and examples, then made the `MNCONT` name and allocatable work arrays portable to Linux. |
!> | 2026-03--04 | Sven Berendsen | 4.6 | Removed DEC dependencies and modernised declarations, interfaces, and control flow while preserving the component algorithms. |
!> | 2026-05 | Sven Berendsen | 4.6 | Moved large work arrays to heap storage and repaired current allocation/runtime failures. |
!> | 2026-09-10 | SvB | - | Split out of MNmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE mn_validation

   USE error_reporting, ONLY: RAISE_ERROR
   USE input_validation, ONLY: ALCHK, ALCHKI
   USE mn_state, ONLY: chum1, clit1, cman1, dummy4, namm1, nlit1, nman1, plup

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: mnerr0, mnerr1, mnerr2, mnerr3, mnerr4

CONTAINS

!> @brief Checks fixed MN array dimensions, entity counts, and selected file units.
!>
!> `mnerr0` validates the static bounds needed before MN arrays are used.
!>
!> | Group | Checks |
!> | --- | --- |
!> | Fixed array limits | `LLEE >= NCETOP`; `NCONEE >= NCON`; `NELEE >= NEL`; `NLFEE >= max(1, NLF)`; `NLYREE > 0`; `NSEE >= NS`; `NVEE >= NV`; `NXEE >= NX` and `NXEE <= 9999`; `NMNEEE > 0`; `NMNTEE > 0`. |
!> | Entity counts | `0 <= NLF < NEL`; `min(NCETOP, NS, NV) > 0`; `min(NX, NY) > 0`. |
!> | Contaminant contract | MN is coupled to exactly one contaminant species: `NCON == 1`. |
!> | File units | Only `MND`, `MNFC`, `MNFN`, and `MNPR` are checked here, and all must be non-negative. |
!>
!> Detailed failures use errors `3020`-`3033`; any failure is followed by
!> fatal summary error `3010`.
   SUBROUTINE MNERR0(LLEE, MND, MNFC, MNFN, MNPR, NCETOP, NCON, NCONEE, NEL, NELEE, NLF, NLFEE, NLYREE, NMNEEE, NMNTEE, NS, NSEE, NV, NVEE, NX, NXEE, NY)

      IMPLICIT NONE

      ! Input arguments
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: MND  !! Static MND input unit.
      INTEGER, INTENT(IN) :: MNFC  !! Scheduled carbon-addition input unit.
      INTEGER, INTENT(IN) :: MNFN  !! Scheduled nitrogen-addition input unit.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NCON  !! Number of contaminant species coupled to MN.
      INTEGER, INTENT(IN) :: NCONEE  !! Contaminant-species array dimension.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NLYREE  !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NMNEEE  !! Maximum number of MN category entries.
      INTEGER, INTENT(IN) :: NMNTEE  !! Maximum number of MN table entries.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NSEE  !! Soil-type array dimension.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NV  !! Number of vegetation types.
      INTEGER, INTENT(IN) :: NVEE  !! Vegetation-type array dimension.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2

      INTEGER, PARAMETER :: IUNDEF = 0

      INTEGER :: NERR
      INTEGER :: IDUMS(1), IDUMO(1)
      LOGICAL :: LDUM1(1)

      ! Replaced implicitly-saved DATA blocks with proper PARAMETER arrays
      INTEGER, PARAMETER :: IZERO(1) = [0]
      INTEGER, PARAMETER :: IONE(1) = [1]

      !-------------------------------------------------------------------*

      ! 0. preliminaries
      ! ----------------
      ! initialize local counter
      NERR = 0

      ! 1. array sizes
      ! --------------

      ! llee
      IDUMS(1) = LLEE
      IDUMO(1) = NCETOP
      CALL ALCHKI(ERR, 3020, MNPR, 1, 1, IUNDEF, IUNDEF, 'llee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! nconee
      IDUMS(1) = NCONEE
      IDUMO(1) = NCON
      CALL ALCHKI(ERR, 3021, MNPR, 1, 1, IUNDEF, IUNDEF, 'nconee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! nelee
      IDUMS(1) = NELEE
      IDUMO(1) = NEL
      CALL ALCHKI(ERR, 3022, MNPR, 1, 1, IUNDEF, IUNDEF, 'nelee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! nlfee
      IDUMS(1) = NLFEE
      IDUMO(1) = MAX(1, NLF)
      CALL ALCHKI(ERR, 3023, MNPR, 1, 1, IUNDEF, IUNDEF, 'nlfee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! nlyree
      IDUMS(1) = NLYREE
      CALL ALCHKI(ERR, 3024, MNPR, 1, 1, IUNDEF, IUNDEF, 'nlyree', 'GT', IZERO, IDUMS, NERR, LDUM1)

      ! nsee
      IDUMS(1) = NSEE
      IDUMO(1) = NS
      CALL ALCHKI(ERR, 3025, MNPR, 1, 1, IUNDEF, IUNDEF, 'nsee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! nvee
      IDUMS(1) = NVEE
      IDUMO(1) = NV
      CALL ALCHKI(ERR, 3026, MNPR, 1, 1, IUNDEF, IUNDEF, 'nvee', 'GE', IDUMO, IDUMS, NERR, LDUM1)

      ! nxee
      IDUMS(1) = NXEE
      IDUMO(1) = NX
      CALL ALCHKI(ERR, 3027, MNPR, 1, 1, IUNDEF, IUNDEF, 'nxee', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      IDUMO(1) = 9999
      CALL ALCHKI(ERR, 3027, MNPR, 1, 1, IUNDEF, IUNDEF, 'nxee', 'LE', IDUMO, IDUMS, NERR, LDUM1)

      ! nmneee
      IDUMS(1) = NMNEEE
      CALL ALCHKI(ERR, 3028, MNPR, 1, 1, IUNDEF, IUNDEF, 'nmneee', 'GT', IZERO, IDUMS, NERR, LDUM1)

      ! nmntee
      IDUMS(1) = NMNTEE
      CALL ALCHKI(ERR, 3028, MNPR, 1, 1, IUNDEF, IUNDEF, 'nmntee', 'GT', IZERO, IDUMS, NERR, LDUM1)

      ! 2. number of entities
      ! ---------------------

      ! nlf
      IDUMS(1) = NLF
      IDUMO(1) = NEL
      CALL ALCHKI(ERR, 3029, MNPR, 1, 1, IUNDEF, IUNDEF, 'nlf', 'GE', IZERO, IDUMS, NERR, LDUM1)
      CALL ALCHKI(ERR, 3029, MNPR, 1, 1, IUNDEF, IUNDEF, 'nlf', 'LT', IDUMO, IDUMS, NERR, LDUM1)

      ! ncetop,ns,nv
      IDUMS(1) = MIN(NCETOP, NS, NV)
      CALL ALCHKI(ERR, 3030, MNPR, 1, 1, IUNDEF, IUNDEF, '[ncetop,ns,nv]', 'GT', IZERO, IDUMS, NERR, LDUM1)

      ! nx, ny
      IDUMS(1) = MIN(NX, NY)
      CALL ALCHKI(ERR, 3031, MNPR, 1, 1, IUNDEF, IUNDEF, '[ nx, ny ]', 'GT', IZERO, IDUMS, NERR, LDUM1)

      ! ncon
      IDUMS(1) = NCON
      CALL ALCHKI(ERR, 3032, MNPR, 1, 1, IUNDEF, IUNDEF, 'ncon', 'EQ', IONE, IDUMS, NERR, LDUM1)

      ! 3. unit numbers
      ! ---------------

      ! mnd,mnfc,mnfn,mnpr
      IDUMS(1) = MIN(MND, MNFC, MNFN, MNPR)
      CALL ALCHKI(ERR, 3033, MNPR, 1, 1, IUNDEF, IUNDEF, '[mnd,mnpr]', 'GE', IZERO, IDUMS, NERR, LDUM1)

      ! 4. epilogue
      ! -----------
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(FATAL, 3010, MNPR, 0, 0, 'error(s) detected while checking cm-mn interface variables')
      END IF

   END SUBROUTINE MNERR0

!> @brief Checks the static contaminant-to-MN interface variables.
!>
!> `mnerr1` validates the spatial indexing and soil-column geometry handed to
!> the mineral-nitrogen component before initialisation.
!>
!> | Group | Checks |
!> | --- | --- |
!> | Grid and bank identities | Active grid entries in `ICMXY`, plus both bank elements for each link when `BEXBK` is true, must account for exactly `NEL-NLF` column elements (`2075`). Every model element must be represented once (`2076`). |
!> | Bank neighbours | If the identity check passed and banks exist, each link must have at least one bank with an active grid neighbour (`2079`). The checked face is `2*bank`, decremented for north-south links, and the neighbour is read from `ICMREF(element,face,2)`. |
!> | Reference values | `D0 > 0` and `Z2 > 0`. |
!> | Soil properties | Soil porosity satisfies `0 < VSPOR(soil) <= 1`. |
!> | Column geometry | Land-column `DXQQ` and `DYQQ` are positive; `1 <= NLYR <= NLYREE`; `NLYRBT` is strictly increasing and the top-layer boundary equals `NCETOP+1`; `NTSOIL` is in `1:NS`; `0 < NCOLMB <= NCETOP`; active `DELTAZ` values are positive; `ZVSNOD(nce+1,iel) > ZVSNOD(nce,iel)`. |
!> | Time | Initial simulation time `TIH >= 0`. |
!>
!> Detailed interface failures use errors `3035`-`3046`; any failure is followed
!> by fatal summary error `3011`.
   SUBROUTINE MNERR1(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NLFEE, NLYREE, NS, NX, NXEE, NY, ICMBK, ICMREF, &
      ICMXY, NCOLMB, NLYR, NLYRBT, NTSOIL, D0, TIH, Z2, DXQQ, DYQQ, VSPOR, DELTAZ, ZVSNOD, &
      BEXBK, LINKNS, DUMMY2, DUMMY3, IDUM, IDUM1X, LDUM, LDUM2)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links.
      INTEGER, INTENT(IN) :: NLFEE  !! Link-array dimension.
      INTEGER, INTENT(IN) :: NLYREE  !! Soil-layer array dimension.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NX  !! Number of grid columns.
      INTEGER, INTENT(IN) :: NXEE  !! Grid-column array dimension.
      INTEGER, INTENT(IN) :: NY  !! Number of grid rows.
      INTEGER, INTENT(IN) :: ICMBK(NLFEE, 2)  !! Bank-element numbers for each channel link.
      INTEGER, INTENT(IN) :: ICMREF(NELEE, 4, 2:2)  !! Neighbour reference map used to validate bank adjacency.
      INTEGER, INTENT(IN) :: ICMXY(NXEE, NY)  !! Element number at each grid location.
      INTEGER, INTENT(IN) :: NLYRBT(NEL, NLYREE)  !! Bottom cell index of each soil layer.
      INTEGER, INTENT(IN) :: NTSOIL(NEL, NLYREE)  !! Soil type index for each element layer.
      DOUBLE PRECISION, INTENT(IN) :: D0  !! Reference diffusion/dispersion scale used by CM.
      DOUBLE PRECISION, INTENT(IN) :: TIH  !! Initial simulation time in hours.
      DOUBLE PRECISION, INTENT(IN) :: Z2  !! Vertical length scale used by CM and MN temperature diffusion.
      LOGICAL, INTENT(IN) :: BEXBK  !! True when bank elements are represented.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE)  !! True for north-south channel links.

      ! Input/Output Arrays (Tested by ALCHK/ALCHKI; subject to internal data reset)
      INTEGER, INTENT(INOUT) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      INTEGER, INTENT(INOUT) :: NLYR(NELEE)  !! Number of soil layers in each element.
      DOUBLE PRECISION, INTENT(INOUT) :: DXQQ(NELEE)  !! Element width.
      DOUBLE PRECISION, INTENT(INOUT) :: DYQQ(NELEE)  !! Element length.
      DOUBLE PRECISION, INTENT(INOUT) :: VSPOR(NS)  !! Soil porosity by soil type.
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAZ(LLEE, NEL)  !! Cell thickness by cell and element.
      DOUBLE PRECISION, INTENT(INOUT) :: ZVSNOD(LLEE, NEL)  !! Vertical node elevation/depth by cell and element.

      ! Workspace arguments (INTENT(INOUT) as they are used for scratch space)
      INTEGER, INTENT(INOUT) :: DUMMY2(NLYREE, NELEE)  !! Integer workspace for layer membership checks.
      INTEGER, INTENT(INOUT) :: DUMMY3(NLYREE)  !! Integer workspace for layer checks.
      INTEGER, INTENT(INOUT) :: IDUM(NELEE)  !! Integer workspace for element accounting.
      INTEGER, INTENT(INOUT) :: IDUM1X(-1:NEL + 1)  !! Integer workspace for element identity checks.
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)  !! Logical workspace for element accounting.
      LOGICAL, INTENT(INOUT) :: LDUM2(LLEE)  !! Logical workspace for cell/layer checks.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      INTEGER :: BANK, BOTLYR, COUNT, FACE
      INTEGER :: IADJ, ICOL1, IEL, IX, IY
      INTEGER :: LINK, NCE, NCEBOT, NCOL, NELP
      INTEGER :: NERR, NLAYER, TOPLYR
      INTEGER :: IDUM1(2)
      DOUBLE PRECISION :: DUMS(1)
      LOGICAL :: BKXYOK

      INTEGER, PARAMETER :: IZERO_ARR(1) = [0], IONE_ARR(1) = [1]
      DOUBLE PRECISION, PARAMETER :: ZERO_ARR(1) = [0.0D0], ONE_ARR(1) = [1.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0
      INTEGER, PARAMETER :: IUNDEF = 0

      !-------------------------------------------------------------------*

      ! 0. preliminaries
      ! ----------------
      NERR = 0
      ICOL1 = NLF + 1
      NELP = NEL + 1

      ! 1. index arrays
      ! ---------------

      ! icmbk, icmxy
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

      CALL ALCHKI(ERR, 2075, MNPR, 1, 1, IUNDEF, IUNDEF, '#_column_elements', 'EQ', IDUM1, IDUM1X(0:0), NERR, LDUM)
      CALL ALCHKI(ERR, 2076, MNPR, 1, NEL, IUNDEF, IUNDEF, 'element_count(iel)', 'EQ', IONE_ARR, IDUM1X(1:NEL), NERR, LDUM)

      BKXYOK = (COUNT == NERR)

      ! icmref (bank element neighbours)
      IF (NLF > 0 .AND. BEXBK .AND. BKXYOK) THEN
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
         CALL ALCHKI(ERR, 2079, MNPR, 1, NLF, IUNDEF, IUNDEF, '#_grids_neighbouring_banks(link)', 'GT', IZERO_ARR, IDUM, NERR, LDUM)
      END IF

      ! 2. contaminant reference values
      ! -------------------------------

      ! d0
      DUMS(1) = D0
      CALL ALCHK(ERR, 3035, MNPR, 1, 1, IUNDEF, IUNDEF, 'd0', 'GT', ZERO_ARR, ZERO_VAL, DUMS, NERR, LDUM)

      ! z2
      DUMS(1) = Z2
      CALL ALCHK(ERR, 3036, MNPR, 1, 1, IUNDEF, IUNDEF, 'z2', 'GT', ZERO_ARR, ZERO_VAL, DUMS, NERR, LDUM)

      ! 3. soil properties
      ! ------------------
      ! vspor
      CALL ALCHK(ERR, 3037, MNPR, 1, NS, IUNDEF, IUNDEF, 'vspor(soil)', 'LE', ONE_ARR, ZERO_VAL, VSPOR, NERR, LDUM)
      CALL ALCHK(ERR, 3037, MNPR, 1, NS, IUNDEF, IUNDEF, 'vspor(soil)', 'GT', ZERO_ARR, ZERO_VAL, VSPOR, NERR, LDUM)

      ! 4. column properties
      ! --------------------

      ! dxqq
      CALL ALCHK(ERR, 3039, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'dxqq(iel)', 'GT', ZERO_ARR, ZERO_VAL, DXQQ(ICOL1:NEL), NERR, LDUM)
      ! dyqq
      CALL ALCHK(ERR, 3039, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'dyqq(iel)', 'GT', ZERO_ARR, ZERO_VAL, DYQQ(ICOL1:NEL), NERR, LDUM)

      ! nlyr
      COUNT = NERR
      IDUM1(1) = 1
      CALL ALCHKI(ERR, 3041, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'nlyr(iel)', 'GE', IDUM1, NLYR(ICOL1:NEL), NERR, LDUM)
      IDUM1(1) = NLYREE
      CALL ALCHKI(ERR, 3041, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'nlyr(iel)', 'LE', IDUM1, NLYR(ICOL1:NEL), NERR, LDUM)

      ! nlyrbt
      IF (COUNT == NERR) THEN
         DO NLAYER = 1, NLYREE
            DO IEL = 1, NEL
               DUMMY2(NLAYER, IEL) = NLYRBT(IEL, NLAYER)
            END DO
         END DO
         DO IEL = ICOL1, NEL
            BOTLYR = 1
            TOPLYR = NLYR(IEL)
            DUMMY3(BOTLYR) = 0
            DO NLAYER = BOTLYR, TOPLYR
               DUMMY3(NLAYER + 1) = DUMMY2(NLAYER, IEL)
            END DO

            CALL ALCHKI(ERR, 3042, MNPR, BOTLYR, TOPLYR + 1, IEL, IUNDEF, 'nlyrbt[nlyr,iel]', 'GTa', DUMMY3(BOTLYR:TOPLYR + 1), &
               DUMMY2(BOTLYR:TOPLYR + 1, IEL), NERR, LDUM2)

            IDUM1(1) = NCETOP + 1
            CALL ALCHKI(ERR, 3042, MNPR, TOPLYR, TOPLYR, IEL, IUNDEF, 'nlyrbt[toplyr,iel]', 'EQ', IDUM1(1:1), DUMMY2(TOPLYR+1:TOPLYR+1, IEL), NERR, LDUM2)
         END DO
      END IF

      ! ntsoil
      IF (COUNT == NERR) THEN
         DO NLAYER = 1, NLYREE
            DO IEL = 1, NEL
               DUMMY2(NLAYER, IEL) = NTSOIL(IEL, NLAYER)
            END DO
         END DO
         DO IEL = ICOL1, NEL
            BOTLYR = 1
            TOPLYR = NLYR(IEL)
            CALL ALCHKI(ERR, 3043, MNPR, BOTLYR, TOPLYR, IEL, IUNDEF, 'ntsoil[nlyr,iel]', 'GT', IZERO_ARR, DUMMY2(BOTLYR:TOPLYR, IEL), NERR, LDUM2)
            IDUM1(1) = NS
            CALL ALCHKI(ERR, 3043, MNPR, BOTLYR, TOPLYR, IEL, IUNDEF, 'ntsoil[nlyr,iel]', 'LE', IDUM1(1:1), DUMMY2(BOTLYR:TOPLYR, IEL), NERR, LDUM2)
         END DO
      END IF

      ! ncolmb
      IDUM1(1) = NCETOP
      CALL ALCHKI(ERR, 3044, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ncolmb(iel)', 'GT', IZERO_ARR, NCOLMB(ICOL1:NEL), NERR, LDUM)
      CALL ALCHKI(ERR, 3044, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ncolmb(iel)', 'LE', IDUM1, NCOLMB(ICOL1:NEL), NERR, LDUM)

      ! deltz,zvsnod
      DO IEL = ICOL1, NEL
         DO NCE = NCOLMB(IEL), NCETOP
            DUMMY4(NCE, IEL) = DELTAZ(NCE, IEL)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3045, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'deltaz[ncl,iel]', 'GT', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO
      DO IEL = ICOL1, NEL
         DO NCE = NCOLMB(IEL), NCETOP - 1
            DUMS(1) = ZVSNOD(NCE, IEL)
            DUMMY4(NCE + 1, IEL) = ZVSNOD(NCE + 1, IEL)
            CALL ALCHK(ERR, 3045, MNPR, NCE + 1, NCE + 1, IEL, IUNDEF, 'zvsnod', 'GT', DUMS(1:1), ZERO_VAL, DUMMY4(NCE+1:NCE+1, IEL), NERR, LDUM2)
         END DO
      END DO

      ! 5. time properties
      ! ------------------
      ! tih
      DUMS(1) = TIH
      CALL ALCHK(ERR, 3046, MNPR, 1, 1, IUNDEF, IUNDEF, 'tih', 'GE', ZERO_ARR, ZERO_VAL, DUMS, NERR, LDUM)

      ! 6. epilogue
      ! -----------
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(FATAL, 3011, MNPR, 0, 0, 'error(s) detected while checking static/initial interface')
      END IF

   END SUBROUTINE MNERR1

!> @brief Checks static mineral-nitrogen input read by [[mnred1]].
!>
!> `mnerr2` validates the nitrogen and carbon data file after [[mnred1]] has
!> loaded it. Land-column checks run over elements `NLF+1:NEL`.
!>
!> | Group | Checks |
!> | --- | --- |
!> | Uptake and immobilisation | `KUAMM`, `KPLAMM`, `KUNIT`, and `KPLNIT` are non-negative. |
!> | Carbon cycling scalars | `0 <= FE <= 1`, `0 <= FH <= 1`, and `CNRBIO`, `CNRHUM`, and `CNRLIT` are positive. The initial-carbon litter fraction `CLITFR` must be in `0:1`. |
!> | Temperature and deposition scalars | `Q10M` and `Q10N` are non-negative; ammonium and nitrate dry/wet deposition rates are non-negative; `MNCREF > 0`. |
!> | Initial carbon | If `ISICCD` is true, decay-function inputs require `CTOTTP >= 0` and `DCHLF > 0`. Otherwise `CELEM > 0`, initial-carbon table depths start at zero and increase, and table concentrations are non-negative. |
!> | Initial ammonium | If `ISIAMD` is true, decay-function inputs require `NAMTOP >= 0` and `DAMHLF > 0`. Otherwise `NAELEM > 0`, initial-ammonium table depths start at zero and increase, and table concentrations are non-negative. |
!> | Depth-varying process tables | Category ids for `KHUM`, `KLIT`, `KMAN`, `KNIT`, `KVOL`, `KD1`, and `KD2` are positive. Their depth tables start at zero, subsequent depths increase, and table values are non-negative. |
!> | Ammonium adsorption and active depth | `KDDSOL(soil) >= 0` and `NBOTCE < NCETOP`. |
!>
!> Detailed failures use errors `3048`-`3064`; any failure is followed by fatal
!> summary error `3012`.
   SUBROUTINE MNERR2(MNPR, NBOTCE, NCETOP, NEL, NELEE, NLF, NMN15E, NMN17E, NMN19E, NMN21E, NMN23E, NMN25E, NMN27E, NMN43E, NMN53E, &
      NMNEEE, NMNTEE, NS, CELEM, KD1ELM, KD2ELM, KHELEM, KLELEM, KMELEM, KNELEM, KVELEM, NAELEM, NMN15T, NMN17T, NMN19T, NMN21T, &
      NMN23T, NMN25T, NMN27T, NMN43T, NMN53T, AMMDDR, AMMWDR, CLITFR, CNRBIO, CNRHUM, CNRLIT, FE, FH, GNN, KPLAMM, KPLNIT, KUAMM, KUNIT, &
      MNCREF, NITDDR, NITWDR, Q10M, Q10N, CCONC, CDPTH, CTOTTP, DAMHLF, DCHLF, KD1CNC, KD1DTH, KD2CNC, KD2DTH, KDDSOL, KHCONC, KHDPTH, &
      KLCONC, KLDPTH, KMCONC, KMDPTH, KNCONC, KNDPTH, KVCONC, KVDPTH, NACONC, NADPTH, NAMTOP, ISICCD, ISIAMD, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NBOTCE  !! Lowest cell included when bottom-cell truncation is active.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column checks.
      INTEGER, INTENT(IN) :: NMN15E  !! Number of humus category entries.
      INTEGER, INTENT(IN) :: NMN17E  !! Number of litter category entries.
      INTEGER, INTENT(IN) :: NMN19E  !! Number of manure category entries.
      INTEGER, INTENT(IN) :: NMN21E  !! Number of nitrification category entries.
      INTEGER, INTENT(IN) :: NMN23E  !! Number of volatilisation category entries.
      INTEGER, INTENT(IN) :: NMN25E  !! Number of KD1 denitrification category entries.
      INTEGER, INTENT(IN) :: NMN27E  !! Number of KD2 denitrification category entries.
      INTEGER, INTENT(IN) :: NMN43E  !! Number of initial-carbon category entries.
      INTEGER, INTENT(IN) :: NMN53E  !! Number of initial-ammonium category entries.
      INTEGER, INTENT(IN) :: NMNEEE  !! Maximum number of MN category entries.
      INTEGER, INTENT(IN) :: NMNTEE  !! Maximum number of MN table entries.
      INTEGER, INTENT(IN) :: NS  !! Number of soil types.
      INTEGER, INTENT(IN) :: NMN15T(NMNEEE)  !! Humus table length by category.
      INTEGER, INTENT(IN) :: NMN17T(NMNEEE)  !! Litter table length by category.
      INTEGER, INTENT(IN) :: NMN19T(NMNEEE)  !! Manure table length by category.
      INTEGER, INTENT(IN) :: NMN21T(NMNEEE)  !! Nitrification table length by category.
      INTEGER, INTENT(IN) :: NMN23T(NMNEEE)  !! Volatilisation table length by category.
      INTEGER, INTENT(IN) :: NMN25T(NMNEEE)  !! KD1 table length by category.
      INTEGER, INTENT(IN) :: NMN27T(NMNEEE)  !! KD2 table length by category.
      INTEGER, INTENT(IN) :: NMN43T(NMNEEE)  !! Initial-carbon table length by category.
      INTEGER, INTENT(IN) :: NMN53T(NMNEEE)  !! Initial-ammonium table length by category.
      DOUBLE PRECISION, INTENT(IN) :: AMMDDR  !! Dry ammonium deposition rate.
      DOUBLE PRECISION, INTENT(IN) :: AMMWDR  !! Wet ammonium deposition coefficient.
      DOUBLE PRECISION, INTENT(IN) :: CLITFR  !! Fraction of initial organic carbon assigned to litter.
      DOUBLE PRECISION, INTENT(IN) :: CNRBIO  !! Biomass carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: CNRHUM  !! Humus carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: CNRLIT  !! Litter carbon-to-nitrogen ratio.
      DOUBLE PRECISION, INTENT(IN) :: FE  !! Efficiency fraction for organic carbon turnover.
      DOUBLE PRECISION, INTENT(IN) :: FH  !! Humification fraction.
      DOUBLE PRECISION, INTENT(IN) :: GNN  !! Nonlinear ammonium adsorption exponent.
      DOUBLE PRECISION, INTENT(IN) :: KPLAMM  !! First-order ammonium plant-uptake limit.
      DOUBLE PRECISION, INTENT(IN) :: KPLNIT  !! First-order nitrate plant-uptake limit.
      DOUBLE PRECISION, INTENT(IN) :: KUAMM  !! First-order ammonium immobilisation limit.
      DOUBLE PRECISION, INTENT(IN) :: KUNIT  !! First-order nitrate immobilisation limit.
      DOUBLE PRECISION, INTENT(IN) :: MNCREF  !! Reference nitrogen concentration.
      DOUBLE PRECISION, INTENT(IN) :: NITDDR  !! Dry nitrate deposition rate.
      DOUBLE PRECISION, INTENT(IN) :: NITWDR  !! Wet nitrate deposition coefficient.
      DOUBLE PRECISION, INTENT(IN) :: Q10M  !! Q10 coefficient for mineralisation.
      DOUBLE PRECISION, INTENT(IN) :: Q10N  !! Q10 coefficient for nitrification.
      LOGICAL, INTENT(IN) :: ISICCD  !! True when initial carbon uses decay-function input.
      LOGICAL, INTENT(IN) :: ISIAMD  !! True when initial ammonium uses decay-function input.

      ! Arguments tested by ALCHK/ALCHKI (Strict INTENT(INOUT) to satisfy dummy arguments)
      INTEGER, INTENT(INOUT) :: CELEM(NLF + 1:NEL)  !! Initial-carbon category by element.
      INTEGER, INTENT(INOUT) :: KD1ELM(NLF + 1:NEL)  !! KD1 denitrification category by element.
      INTEGER, INTENT(INOUT) :: KD2ELM(NLF + 1:NEL)  !! KD2 denitrification category by element.
      INTEGER, INTENT(INOUT) :: KHELEM(NLF + 1:NEL)  !! Humus decomposition category by element.
      INTEGER, INTENT(INOUT) :: KLELEM(NLF + 1:NEL)  !! Litter decomposition category by element.
      INTEGER, INTENT(INOUT) :: KMELEM(NLF + 1:NEL)  !! Manure decomposition category by element.
      INTEGER, INTENT(INOUT) :: KNELEM(NLF + 1:NEL)  !! Nitrification category by element.
      INTEGER, INTENT(INOUT) :: KVELEM(NLF + 1:NEL)  !! Volatilisation category by element.
      INTEGER, INTENT(INOUT) :: NAELEM(NLF + 1:NEL)  !! Initial-ammonium category by element.
      DOUBLE PRECISION, INTENT(INOUT) :: CCONC(NMNEEE, NMNTEE)  !! Initial-carbon profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: CDPTH(NMNEEE, NMNTEE)  !! Initial-carbon profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: CTOTTP(NLF + 1:NEL)  !! Top total-carbon value for decay initialisation.
      DOUBLE PRECISION, INTENT(INOUT) :: DAMHLF(NLF + 1:NEL)  !! Ammonium decay half-depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: DCHLF(NLF + 1:NEL)  !! Carbon decay half-depth by element.
      DOUBLE PRECISION, INTENT(INOUT) :: KD1CNC(NMNEEE, NMNTEE)  !! KD1 denitrification profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KD1DTH(NMNEEE, NMNTEE)  !! KD1 denitrification profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KD2CNC(NMNEEE, NMNTEE)  !! KD2 denitrification profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KD2DTH(NMNEEE, NMNTEE)  !! KD2 denitrification profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KDDSOL(NS)  !! Soil ammonium adsorption coefficient.
      DOUBLE PRECISION, INTENT(INOUT) :: KHCONC(NMNEEE, NMNTEE)  !! Humus decomposition profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KHDPTH(NMNEEE, NMNTEE)  !! Humus decomposition profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KLCONC(NMNEEE, NMNTEE)  !! Litter decomposition profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KLDPTH(NMNEEE, NMNTEE)  !! Litter decomposition profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KMCONC(NMNEEE, NMNTEE)  !! Manure decomposition profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KMDPTH(NMNEEE, NMNTEE)  !! Manure decomposition profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KNCONC(NMNEEE, NMNTEE)  !! Nitrification profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KNDPTH(NMNEEE, NMNTEE)  !! Nitrification profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: KVCONC(NMNEEE, NMNTEE)  !! Volatilisation profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: KVDPTH(NMNEEE, NMNTEE)  !! Volatilisation profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: NACONC(NMNEEE, NMNTEE)  !! Initial-ammonium profile values.
      DOUBLE PRECISION, INTENT(INOUT) :: NADPTH(NMNEEE, NMNTEE)  !! Initial-ammonium profile depths.
      DOUBLE PRECISION, INTENT(INOUT) :: NAMTOP(NLF + 1:NEL)  !! Top ammonium value for decay initialisation.

      ! Workspace arguments
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)  !! Logical workspace for element checks.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2, WARN = 3
      INTEGER :: ICOL1, NELMTY, NERR, NTAB

      ! Safe scalar passing arrays
      INTEGER :: IDUMS(1), IDUMO(1)
      DOUBLE PRECISION :: PREVDP_ARR(1), DUMS_ARR(1)

      INTEGER, PARAMETER :: IZERO_ARR(1) = [0]
      DOUBLE PRECISION, PARAMETER :: ZERO_ARR(1) = [0.0D0], ONE_ARR(1) = [1.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0
      INTEGER, PARAMETER :: IUNDEF = 0

      !-------------------------------------------------------------------*

      ! 0. preliminaries
      ! ----------------
      NERR = 0
      ICOL1 = NLF + 1

      ! 1. spatially constant decomposition parameters
      ! ---------------------------
      ! kuamm,kplamm
      DUMS_ARR(1) = MIN(KUAMM, KPLAMM)
      CALL ALCHK(ERR, 3050, MNPR, 1, 1, IUNDEF, IUNDEF, '[ kuamm,kplamm ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      ! kunit,kplnit
      DUMS_ARR(1) = MIN(KUNIT, KPLNIT)
      CALL ALCHK(ERR, 3050, MNPR, 1, 1, IUNDEF, IUNDEF, '[ kunit,kplnit ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      ! 2. other parameters
      ! -------------------
      ! fe, fh
      DUMS_ARR(1) = MIN(FE, FH)
      CALL ALCHK(ERR, 3055, MNPR, 1, 1, IUNDEF, IUNDEF, '[ fe,fh ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)
      DUMS_ARR(1) = MAX(FE, FH)
      CALL ALCHK(ERR, 3055, MNPR, 1, 1, IUNDEF, IUNDEF, '[ fe,fh ]', 'LE', ONE_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      ! cnrbio,cnrhum
      DUMS_ARR(1) = MIN(CNRBIO, CNRHUM)
      CALL ALCHK(ERR, 3056, MNPR, 1, 1, IUNDEF, IUNDEF, '[ cnrbio,cnrhum ]', 'GT', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      ! q10m, q10n
      DUMS_ARR(1) = MIN(Q10M, Q10N)
      CALL ALCHK(ERR, 3057, MNPR, 1, 1, IUNDEF, IUNDEF, '[ q10m, q10n ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      ! ammddr, ammwdr
      DUMS_ARR(1) = MIN(AMMDDR, AMMWDR)
      CALL ALCHK(ERR, 3058, MNPR, 1, 1, IUNDEF, IUNDEF, '[ ammddr,ammwdr ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      ! nitddr, nitwdr
      DUMS_ARR(1) = MIN(NITDDR, NITWDR)
      CALL ALCHK(ERR, 3058, MNPR, 1, 1, IUNDEF, IUNDEF, '[ nitddr, nitwdr ]', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      ! mncref
      DUMS_ARR(1) = MNCREF
      CALL ALCHK(ERR, 3059, MNPR, 1, 1, IUNDEF, IUNDEF, 'mncref', 'GT', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      ! 3. initial concentrations
      ! -------------------------
      !    * carbon pool
      !    * -----------
      IF (ISICCD) THEN
         ! *ctottp
         CALL ALCHK(ERR, 3060, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ctottp(iel)', 'GE', ZERO_ARR, ZERO_VAL, CTOTTP, NERR, LDUM)
         ! *dchlf
         CALL ALCHK(ERR, 3061, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'dchlf(iel)', 'GT', ZERO_ARR, ZERO_VAL, DCHLF, NERR, LDUM)
      ELSE
         ! *celem
         CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'celem(iel)', 'GT', IZERO_ARR, CELEM, NERR, LDUM)

         ! *cdpth
         DO NELMTY = 1, NMN43E
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'cdpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, CDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
            DO NTAB = 2, NMN43T(NELMTY)
               PREVDP_ARR(1) = CDPTH(NELMTY, NTAB - 1)
               CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'cdpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, CDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
            END DO
         END DO

         ! *cconc
         DO NELMTY = 1, NMN43E
            DO NTAB = 1, NMN43T(NELMTY)
               CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'cconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, CCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
            END DO
         END DO
      END IF

      !  * carbon litter fraction and carbon/nitrogen ratio
      !  clitfr
      DUMS_ARR(1) = CLITFR
      CALL ALCHK(ERR, 3062, MNPR, 1, 1, IUNDEF, IUNDEF, 'clitfr', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)
      CALL ALCHK(ERR, 3062, MNPR, 1, 1, IUNDEF, IUNDEF, 'clitfr', 'LE', ONE_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)
      !  cnrlit
      DUMS_ARR(1) = CNRLIT
      CALL ALCHK(ERR, 3063, MNPR, 1, 1, IUNDEF, IUNDEF, 'cnrlit', 'GT', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      !    * ammonium pool
      !    * -------------
      IF (ISIAMD) THEN
         ! * namtop
         CALL ALCHK(ERR, 3060, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'namtop(iel)', 'GE', ZERO_ARR, ZERO_VAL, NAMTOP, NERR, LDUM)
         ! * damhlf
         CALL ALCHK(ERR, 3061, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'damhlf(iel)', 'GT', ZERO_ARR, ZERO_VAL, DAMHLF, NERR, LDUM)
      ELSE
         ! *naelem
         CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'naelem(iel)', 'GT', IZERO_ARR, NAELEM, NERR, LDUM)

         ! *nadpth
         DO NELMTY = 1, NMN53E
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'nadpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, NADPTH(NELMTY:NELMTY, 1), NERR, LDUM)
            DO NTAB = 2, NMN53T(NELMTY)
               PREVDP_ARR(1) = NADPTH(NELMTY, NTAB - 1)
               CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'nadpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, NADPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
            END DO
         END DO

         ! *naconc
         DO NELMTY = 1, NMN53E
            DO NTAB = 1, NMN53T(NELMTY)
               CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'naconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, NACONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
            END DO
         END DO
      END IF

      ! 4. spatially varying parameters
      ! -------------------------------

      ! 4.1 kh
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'khelem(iel)', 'GT', IZERO_ARR, KHELEM, NERR, LDUM)
      DO NELMTY = 1, NMN15E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'khdpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KHDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN15T(NELMTY)
            PREVDP_ARR(1) = KHDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'khdpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KHDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN15E
         DO NTAB = 1, NMN15T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'khconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KHCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

      ! 4.2 kl
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'klelem(iel)', 'GT', IZERO_ARR, KLELEM, NERR, LDUM)
      DO NELMTY = 1, NMN17E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kldpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KLDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN17T(NELMTY)
            PREVDP_ARR(1) = KLDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kldpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KLDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN17E
         DO NTAB = 1, NMN17T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'klconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KLCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

      ! 4.3 km
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'kmelem(iel)', 'GT', IZERO_ARR, KMELEM, NERR, LDUM)
      DO NELMTY = 1, NMN19E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kmdpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KMDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN19T(NELMTY)
            PREVDP_ARR(1) = KMDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kmdpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KMDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN19E
         DO NTAB = 1, NMN19T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kmconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KMCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

      ! 4.4 kn
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'knelem(iel)', 'GT', IZERO_ARR, KNELEM, NERR, LDUM)
      DO NELMTY = 1, NMN21E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kndpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KNDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN21T(NELMTY)
            PREVDP_ARR(1) = KNDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kndpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KNDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN21E
         DO NTAB = 1, NMN21T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'knconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KNCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

      ! 4.5 kv
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'kvelem(iel)', 'GT', IZERO_ARR, KVELEM, NERR, LDUM)
      DO NELMTY = 1, NMN23E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kvdpth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KVDPTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN23T(NELMTY)
            PREVDP_ARR(1) = KVDPTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kvdpth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KVDPTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN23E
         DO NTAB = 1, NMN23T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kvconc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KVCONC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

      ! 4.6 kd1
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'kd1elm(iel)', 'GT', IZERO_ARR, KD1ELM, NERR, LDUM)
      DO NELMTY = 1, NMN25E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kd1dth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KD1DTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN25T(NELMTY)
            PREVDP_ARR(1) = KD1DTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kd1dth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KD1DTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN25E
         DO NTAB = 1, NMN25T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kd1cnc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KD1CNC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

      ! 4.7 kd2
      CALL ALCHKI(ERR, 3064, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'kd2elm(iel)', 'GT', IZERO_ARR, KD2ELM, NERR, LDUM)
      DO NELMTY = 1, NMN27E
         CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, 1, IUNDEF, 'kd2dth[nmne,1]', 'EQ', ZERO_ARR, ZERO_VAL, KD2DTH(NELMTY:NELMTY, 1), NERR, LDUM)
         DO NTAB = 2, NMN27T(NELMTY)
            PREVDP_ARR(1) = KD2DTH(NELMTY, NTAB - 1)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kd2dth[nmne,ntab]', 'GT', PREVDP_ARR, ZERO_VAL, KD2DTH(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO
      DO NELMTY = 1, NMN27E
         DO NTAB = 1, NMN27T(NELMTY)
            CALL ALCHK(ERR, 3064, MNPR, NELMTY, NELMTY, NTAB, IUNDEF, 'kd2cnc[nmne,ntab]', 'GE', ZERO_ARR, ZERO_VAL, KD2CNC(NELMTY:NELMTY, NTAB), NERR, LDUM)
         END DO
      END DO

      ! 5. ammonium adsorption parameters
      ! ---------------------------------
      !    * kddsol
      CALL ALCHK(ERR, 3048, MNPR, 1, NS, IUNDEF, IUNDEF, 'kddsol(ns)', 'GE', ZERO_ARR, ZERO_VAL, KDDSOL, NERR, LDUM)

      ! 6. bottom cell for nitrogen transformations
      ! -------------------------------------------
      !    * nbotce
      IDUMO(1) = NCETOP
      IDUMS(1) = NBOTCE
      CALL ALCHKI(ERR, 3049, MNPR, 1, 1, IUNDEF, IUNDEF, 'nbotce', 'LT', IDUMO, IDUMS, NERR, LDUM)

      ! 7. epilogue
      ! -----------
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(FATAL, 3012, MNPR, 0, 0, 'error(s) detected whilst checking the static input data')
      END IF

   END SUBROUTINE MNERR2

!> @brief Checks time-dependent MN inputs and updated state variables.
!>
!> `mnerr3` validates the dynamic CM-MN interface over active land-column cells
!> `NCOLMB(element):NCETOP` for elements `NLF+1:NEL`.
!>
!> | Group | Checks |
!> | --- | --- |
!> | Time | `DTUZ > 0`. On the first call only, `UZNOW >= 0`; the later-call monotonic-time check is present in comments but not active. |
!> | Nitrate concentrations | Dynamic-region concentration `CCCC` and dead-space concentration `SSSS` are non-negative. |
!> | Organic pools | Updated humus carbon, litter carbon, manure carbon, litter nitrogen, and manure nitrogen pools are non-negative. |
!> | Ammonium pool | Updated ammonium concentration `NAMM1` is non-negative. |
!> | Soil water and uptake | Current and previous soil-water contents satisfy `0 < VSTHE <= 1` and `0 < VSTHEO <= 1`; plant uptake `PLUP >= 0`. |
!> | Rainfall input | Net precipitation/effective rainfall `PNETTO >= 0` for land-column elements. |
!>
!> Detailed failures use errors `3065`-`3072`; any failure is followed by fatal
!> summary error `3013`.
   SUBROUTINE MNERR3(LLEE, MNPR, NCETOP, NEL, NELEE, NLF, NCOLMB, DTUZ, UZNOW, CCCC, &
      PNETTO, SSSS, VSTHE, VSTHEO, LDUM, LDUM2)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: LLEE  !! Maximum soil-cell dimension.
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NCETOP  !! Top soil-cell index.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column checks.
      INTEGER, INTENT(IN) :: NCOLMB(NELEE)  !! Lowest active soil cell in each land-column element.
      DOUBLE PRECISION, INTENT(IN) :: DTUZ  !! Unsaturated-zone timestep in seconds.
      DOUBLE PRECISION, INTENT(IN) :: UZNOW  !! Current unsaturated-zone simulation time.
      DOUBLE PRECISION, INTENT(IN) :: CCCC(NEL, NCETOP + 1)  !! Dynamic-region nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: SSSS(NEL, NCETOP + 1)  !! Dead-space nitrate concentration.
      DOUBLE PRECISION, INTENT(IN) :: VSTHE(NCETOP, NEL)  !! Current volumetric water content.
      DOUBLE PRECISION, INTENT(IN) :: VSTHEO(NEL, NCETOP + 1)  !! Previous volumetric water content.

      ! Arguments tested directly by ALCHK (Must be INTENT(INOUT) to satisfy dummy arguments)
      DOUBLE PRECISION, INTENT(INOUT) :: PNETTO(NELEE)  !! Net precipitation/effective rainfall by element.

      ! Workspace arguments (INTENT(INOUT) because they act as scratch space)
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)  !! Logical workspace for element checks.
      LOGICAL, INTENT(INOUT) :: LDUM2(LLEE)  !! Logical workspace for cell checks.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      INTEGER :: ICOL1, IEL, NCEBOT, NERR, NCE
      DOUBLE PRECISION :: DUMMY4(NCETOP, NEL)
      DOUBLE PRECISION :: DUMS_ARR(1)

      ! Protected static state variables
      INTEGER, SAVE :: PASS = 0
      DOUBLE PRECISION, SAVE :: UZPREV(1) = [0.0D0]

      DOUBLE PRECISION, PARAMETER :: ZERO_ARR(1) = [0.0D0], ONE_ARR(1) = [1.0D0], THIRTY_ARR(1) = [30.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0
      INTEGER, PARAMETER :: IUNDEF = 0

      !-------------------------------------------------------------------*

      ! 0. preliminaries
      ! ----------------
      NERR = 0
      ICOL1 = NLF + 1
      PASS = PASS + 1

      ! 1. variables
      ! ------------

      ! dtuz
      DUMS_ARR(1) = DTUZ
      CALL ALCHK(ERR, 3065, MNPR, 1, 1, IUNDEF, IUNDEF, 'dtuz', 'GT', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)

      ! uznow
      IF (PASS == 1) THEN
         DUMS_ARR(1) = UZNOW
         CALL ALCHK(ERR, 3066, MNPR, 1, 1, IUNDEF, IUNDEF, 'uznow', 'GE', ZERO_ARR, ZERO_VAL, DUMS_ARR, NERR, LDUM)
         UZPREV(1) = UZNOW
      ELSE
         ! temporarily remove this sb 240925 as it is not compiling
         ! DUMS_ARR(1) = UZNOW
         ! CALL ALCHK(ERR, 3066, MNPR, 1, 1, IUNDEF, IUNDEF, 'uznow', 'gt', UZPREV, ZERO_VAL, DUMS_ARR, NERR, LDUM)
         UZPREV(1) = UZNOW
      END IF

      ! 2. nitrate concentrations
      ! -------------------------

      ! cccc, ssss
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = CCCC(IEL, NCE)
         END DO
      END DO

      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3067, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'cccc[iel,ncl]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = SSSS(IEL, NCE)
         END DO
      END DO

      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3067, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'ssss[iel,ncl]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! 3. organic and inorganic pools
      ! ------------------------------

      ! chum1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = CHUM1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'chum1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! clit1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = CLIT1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'clit1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! cman1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = CMAN1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'cman1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! nlit1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = NLIT1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'nlit1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! nman1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = NMAN1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3068, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'nman1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! namm1
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = NAMM1(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3069, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'namm1[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! 4. soil conditions
      ! ------------------

      ! vsthe
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = VSTHE(NCE, IEL)
         END DO
      END DO

      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3070, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'vsthe[ncl,iel]', 'GT', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
         CALL ALCHK(ERR, 3070, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'vsthe[ncl,iel]', 'LE', ONE_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! vstheo
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = VSTHEO(IEL, NCE)
         END DO
      END DO

      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3070, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'vstheo[ncl,iel]', 'GT', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
         CALL ALCHK(ERR, 3070, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'vstheo[ncl,iel]', 'LE', ONE_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! plup
      DO IEL = 1, NEL
         DO NCE = 1, NCETOP
            DUMMY4(NCE, IEL) = PLUP(IEL, NCE)
         END DO
      END DO
      DO IEL = ICOL1, NEL
         NCEBOT = NCOLMB(IEL)
         CALL ALCHK(ERR, 3071, MNPR, NCEBOT, NCETOP, IEL, IUNDEF, 'plup[ncl,iel]', 'GE', ZERO_ARR, ZERO_VAL, DUMMY4(NCEBOT:NCETOP, IEL), NERR, LDUM2)
      END DO

      ! 5. envoironmental conditions
      ! ----------------------------

      ! pnetto
      CALL ALCHK(ERR, 3072, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'pnetto(iel)', 'GE', ZERO_ARR, ZERO_VAL, PNETTO(ICOL1:NEL), NERR, LDUM)

      ! 6. epilogue
      ! -----------
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(FATAL, 3013, MNPR, 0, 0, 'error(s) detected whilst checking the time dependent' // ' variables from cm -mn interface')
      END IF

   END SUBROUTINE MNERR3

!> @brief Checks time-varying fertiliser and organic addition data from [[mnred2]].
!>
!> `mnerr4` validates only the scheduled additions that are active for the
!> current timestep, over land-column elements `NLF+1:NEL`.
!>
!> | Active flag | Checked records | Bounds |
!> | --- | --- | --- |
!> | `ISADDN` | Total inorganic nitrogen `NTOT`, ammonium fraction `NAMFCT`, nitrogen banding depth `NDPTHB`. | `NTOT >= 0`, `0 <= NAMFCT <= 1`, `NDPTHB >= 0`. |
!> | `ISADDC` | Total carbon `CTOT`, carbon banding depth `CDPTHB`, litter fraction `CLTFCT`, manure fraction `CMNFCT`, litter C:N ratio `CNRAL`, manure C:N ratio `CNRAM`. | `CTOT >= 0`, `CDPTHB >= 0`, `CLTFCT >= 0`, `CMNFCT >= 0`, `CLTFCT+CMNFCT <= 1`; when `CTOT > 0`, both C:N ratios must be positive. |
!>
!> Detailed failures use errors `3080`-`3087`; any failure is followed by fatal
!> summary error `3014`.
   SUBROUTINE MNERR4(MNPR, NEL, NELEE, NLF, CDPTHB, CLTFCT, CMNFCT, CNRAL, CNRAM, CTOT, NAMFCT, NDPTHB, NTOT, ISADDC, ISADDN, &
      DUMMY, LDUM)

      IMPLICIT NONE

      ! Input arguments (Strictly Read-Only)
      INTEGER, INTENT(IN) :: MNPR  !! MN diagnostic output unit.
      INTEGER, INTENT(IN) :: NEL  !! Number of elements.
      INTEGER, INTENT(IN) :: NELEE  !! Element-array dimension.
      INTEGER, INTENT(IN) :: NLF  !! Number of overland/channel links excluded from land-column checks.
      LOGICAL, INTENT(IN) :: ISADDC  !! True when a carbon-addition event is active.
      LOGICAL, INTENT(IN) :: ISADDN  !! True when a nitrogen-addition event is active.

      ! Arguments tested directly by ALCHK (Must be INTENT(INOUT) to satisfy dummy arguments)
      DOUBLE PRECISION, INTENT(INOUT) :: CDPTHB(NLF + 1:NEL)  !! Carbon banding depth.
      DOUBLE PRECISION, INTENT(INOUT) :: CLTFCT(NLF + 1:NEL)  !! Litter fraction of added carbon.
      DOUBLE PRECISION, INTENT(INOUT) :: CMNFCT(NLF + 1:NEL)  !! Manure fraction of added carbon.
      DOUBLE PRECISION, INTENT(INOUT) :: CNRAL(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added litter.
      DOUBLE PRECISION, INTENT(INOUT) :: CNRAM(NLF + 1:NEL)  !! Carbon-to-nitrogen ratio for added manure.
      DOUBLE PRECISION, INTENT(INOUT) :: CTOT(NLF + 1:NEL)  !! Total external carbon addition.
      DOUBLE PRECISION, INTENT(INOUT) :: NAMFCT(NLF + 1:NEL)  !! Ammonium fraction of added inorganic nitrogen.
      DOUBLE PRECISION, INTENT(INOUT) :: NDPTHB(NLF + 1:NEL)  !! Nitrogen banding depth.
      DOUBLE PRECISION, INTENT(INOUT) :: NTOT(NLF + 1:NEL)  !! Total external inorganic nitrogen addition.

      ! Workspace arguments (INTENT(INOUT) because they act as scratch space)
      DOUBLE PRECISION, INTENT(INOUT) :: DUMMY(NELEE)  !! Floating-point workspace for range checks.
      LOGICAL, INTENT(INOUT) :: LDUM(NELEE)  !! Logical workspace for range checks.

      ! Locals etc.
      INTEGER, PARAMETER :: FATAL = 1, ERR = 2
      INTEGER :: ICOL1, IEL, NERR

      DOUBLE PRECISION, PARAMETER :: ONE_ARR(1) = [1.0D0], ZERO_ARR(1) = [0.0D0]
      DOUBLE PRECISION, PARAMETER :: ZERO_VAL = 0.0D0
      INTEGER, PARAMETER :: IUNDEF = 0

      !-------------------------------------------------------------------*

      ! 0. preliminaries
      ! ----------------

      NERR = 0
      ICOL1 = NLF + 1

      ! 1. inorganic fertilizer
      ! -----------------------
      IF (ISADDN) THEN
         ! ntot
         CALL ALCHK(ERR, 3080, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ntot(iel)', 'GE', ZERO_ARR, ZERO_VAL, NTOT, NERR, LDUM)

         ! namfct
         CALL ALCHK(ERR, 3081, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'namfct(iel)', 'GE', ZERO_ARR, ZERO_VAL, NAMFCT, NERR, LDUM)
         CALL ALCHK(ERR, 3081, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'namfct(iel)', 'LE', ONE_ARR, ZERO_VAL, NAMFCT, NERR, LDUM)

         ! ndpthb
         CALL ALCHK(ERR, 3082, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ndpthb(iel)', 'GE', ZERO_ARR, ZERO_VAL, NDPTHB, NERR, LDUM)
      END IF

      ! 2. organic fertilizer
      ! -----------------------
      IF (ISADDC) THEN
         ! ctot
         CALL ALCHK(ERR, 3083, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'ctot(iel)', 'GE', ZERO_ARR, ZERO_VAL, CTOT, NERR, LDUM)

         ! cdpthb
         CALL ALCHK(ERR, 3084, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'cdpthb(iel)', 'GE', ZERO_ARR, ZERO_VAL, CDPTHB, NERR, LDUM)

         ! cltfct
         CALL ALCHK(ERR, 3085, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'cltfct(iel)', 'GE', ZERO_ARR, ZERO_VAL, CLTFCT, NERR, LDUM)

         ! cmnfct
         CALL ALCHK(ERR, 3085, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'cmnfct(iel)', 'GE', ZERO_ARR, ZERO_VAL, CMNFCT, NERR, LDUM)

         ! cmnfct + cltfct
         DO IEL = ICOL1, NEL
            DUMMY(IEL) = CLTFCT(IEL) + CMNFCT(IEL)
         END DO
         CALL ALCHK(ERR, 3086, MNPR, ICOL1, NEL, IUNDEF, IUNDEF, 'cltfct+cmnfct(iel)', 'LE', ONE_ARR, ZERO_VAL, DUMMY(ICOL1:NEL), NERR, LDUM)

         ! cnral, cnram
         DO IEL = ICOL1, NEL
            IF (CTOT(IEL) > 0.0D0) THEN
               CALL ALCHK(ERR, 3087, MNPR, IEL, IEL, IUNDEF, IUNDEF, 'cnral(iel)', 'GT', ZERO_ARR, ZERO_VAL, CNRAL(IEL:IEL), NERR, LDUM)
               CALL ALCHK(ERR, 3087, MNPR, IEL, IEL, IUNDEF, IUNDEF, 'cnram(iel)', 'GT', ZERO_ARR, ZERO_VAL, CNRAM(IEL:IEL), NERR, LDUM)
            END IF
         END DO
      END IF

      ! 3. epilogue
      ! -----------
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(FATAL, 3014, MNPR, 0, 0, 'error(s) detected whilst checking the time dependent'//' fertilizer input variables')
      END IF

   END SUBROUTINE MNERR4

END MODULE mn_validation

