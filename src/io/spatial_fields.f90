!> summary: Expansion of category or grid input to element order.
!> author: AB / RAH, Newcastle University; JE, Newcastle University
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
!>
!> Input is given per category or per grid cell; the model works per element.
!> [[ALALLF]] and [[ALALLI]] perform that expansion for real and integer data,
!> with the private [[ALBANK]] helper filling explicit bank elements, and
!> [[ALSPRD]] spreads a value over the vertical cells of a column.
!>
!> The readers these call are in [[record_readers]]; the grid geometry and the
!> workspace arrays they expand through arrive as arguments rather than
!> imports, which is why this module depends on so little.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of mod_load_filedata; see docs/rename/proposal.md. |
!> @endhistory
MODULE spatial_fields

   USE MOD_PARAMETERS, ONLY: I_P, R8P
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal
   USE float_compare, ONLY: idimje
   USE record_readers, ONLY: ALREAD, ALREDI

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: ALALLF, ALALLI, ALSPRD

CONTAINS

   !> Reads and expands a floating-point element or column element-array.
   !>
   !> `ALALLF` implements the manual's FA/FC composite formats. It first reads
   !> the category count from the section headed by `LINE`, then consumes the
   !> applicable suffixed components and expands them into `AEL`.
   !>
   !> | Category count | Records consumed and result |
   !> |:---------------|:----------------------------|
   !> | Below `MINCAT` | Fatal invalid-option error. |
   !> | Negative but allowed by `MINCAT` | Return immediately; `AEL` is not filled. |
   !> | `0` | For every output component, read optional link values from suffix `a` and an indexed real grid from suffix `b`. |
   !> | `1` | Read one value per output component from suffix `c` and fill every selected element uniformly. |
   !> | Greater than `1` | Read category values from `c`, optional link category codes from `d`, and grid category codes from `e`. |
   !>
   !> `FLAG=0` selects all elements and permits explicit link records;
   !> `FLAG=1` selects column/bank elements `NLF+1:NEL`. `N2` is the number of
   !> values per selected element. For a multi-category field the temporary
   !> category table requires `N2*NUM_CATEGORIES_TYPES <= NELEE`. Active grid
   !> positions are mapped by `ICMXY`; if banks exist, non-uniform gridded data
   !> are copied to them through [[ALBANK]]. This is the legacy SSR74 routine.
   !>
   !> @warning
   !> `FLAG` is assumed to be exactly zero or one, `N2` must be positive, and
   !> `LINE` must fit with its one-character suffix in the eight-character
   !> `NEXT` buffer. These preconditions are not checked. The multi-category
   !> path also relies on `DUMMY` retaining real category values across
   !> integer-only [[ALREAD]] calls, despite `ALREAD` declaring `RDATA` as
   !> `INTENT(OUT)` on every call.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-05-27 | - | - | Initial version. |
   !> | 1994-09-19 | AB/RAH | 3.4.1 | Revised the legacy distributed-array reader. |
   !> | 2026-04-05 | SvB | - | Replaced the uniform-field `ALINIT` call with an equivalent array-slice assignment. |
   !> @endhistory
   SUBROUTINE ALALLF(FLAG, N2, MINCAT, IUNIT, OUNIT, LINE, NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, &
                     NYEE, ICMXY, ICMBK, ICMREF, BEXBK, LINKNS, NUM_CATEGORIES_TYPES, AEL, IDUM, &
                     DUMMY)

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: FLAG !! Target selector: zero for FA/all elements, one for FC/column elements.
      INTEGER(kind=I_P), INTENT(IN) :: N2 !! Number of values per selected element.
      INTEGER(kind=I_P), INTENT(IN) :: MINCAT !! Lowest permitted category-count/special-option value.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Open input unit positioned before the initial section heading.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving warnings and fatal diagnostics.
      INTEGER(kind=I_P), INTENT(IN) :: NEL !! Number of active elements, including links and banks.
      INTEGER(kind=I_P), INTENT(IN) :: NLF !! Number of active channel-link elements.
      INTEGER(kind=I_P), INTENT(IN) :: NX !! Active east-west grid extent.
      INTEGER(kind=I_P), INTENT(IN) :: NY !! Active north-south grid extent.
      INTEGER(kind=I_P), INTENT(IN) :: NELEE !! Element workspace/capacity extent.
      INTEGER(kind=I_P), INTENT(IN) :: NLFEE !! Channel-link capacity extent.
      INTEGER(kind=I_P), INTENT(IN) :: NXEE !! Grid workspace extent in the x direction.
      INTEGER(kind=I_P), INTENT(IN) :: NYEE !! Grid workspace extent in the y direction.
      INTEGER(kind=I_P), INTENT(IN) :: ICMXY(NXEE, NY) !! Active grid-coordinate to element-number map.
      INTEGER(kind=I_P), INTENT(IN) :: ICMBK(NLFEE, 2) !! Bank-element number for each link side.
      INTEGER(kind=I_P), INTENT(IN) :: ICMREF(NELEE, 4, 2:2) !! East/north/west/south adjacent-element references supplied from `AL_G:ICMREF(:,5:8)`.
      LOGICAL, INTENT(IN) :: BEXBK !! True when explicit bank elements exist.
      LOGICAL, INTENT(IN) :: LINKNS(NLF) !! True for north-south links; false for east-west links.
      CHARACTER(LEN=*), INTENT(IN) :: LINE !! Base heading code; suffixed with `a` through `e` as required.

      ! Output arguments
      INTEGER(kind=I_P), INTENT(OUT)  :: NUM_CATEGORIES_TYPES !! Category count or permitted negative special-option value read from `LINE`.
      REAL(kind=R8P), INTENT(INOUT)   :: AEL(1 + NLF*(FLAG/N2):NELEE - (NELEE - NEL)*(1/N2), N2)
         !! Expanded field; integer-valued bounds select active elements for `N2=1` and capacity storage for `N2>1`.

      ! Workspace/Buffer arguments
      INTEGER(kind=I_P), DIMENSION(NXEE*NYEE), INTENT(INOUT) :: IDUM !! Flattened integer grid/category workspace.
      REAL(kind=R8P), DIMENSION(NELEE), INTENT(INOUT)        :: DUMMY !! Real grid/category-value workspace.

      ! Locals, etc
      INTEGER(kind=I_P) :: I1 !! First selected element for the uniform category.
      INTEGER(kind=I_P) :: I2 !! Value-component index.
      INTEGER(kind=I_P) :: ICAT !! Current category code.
      INTEGER(kind=I_P) :: IDUM0 !! Scalar integer placeholder passed to `ALREAD`.
      INTEGER(kind=I_P) :: IEL !! Current mapped element number.
      INTEGER(kind=I_P) :: LN !! Length of `LINE` plus its one-character suffix.
      INTEGER(kind=I_P) :: N !! Number of elements filled by a uniform category.
      INTEGER(kind=I_P) :: X !! Grid x index.
      INTEGER(kind=I_P) :: XY0 !! Offset of the current row in flattened `IDUM`/`DUMMY` storage.
      INTEGER(kind=I_P) :: Y !! Grid y index.
      LOGICAL :: BLINK !! True when the selected FA form includes explicit link records.
      CHARACTER :: CDUM !! Character placeholder passed to `ALREAD`.
      CHARACTER(len=132) :: MSG !! Fatal-error message buffer.
      CHARACTER(len=8) :: NEXT !! Suffixed component heading code.

      ! Code =================================================================

      ! -------------
      ! Preliminaries
      ! -------------
      !
      ! Initialization
      LN = LEN(LINE) + 1
      BLINK = NLF > 0 .AND. FLAG == 0

      ! Find out how many categories ( if any )
      CALL ALREAD(2, IUNIT, OUNIT, LINE, 1, 1, IDUM0, CDUM, IDUM, DUMMY)
      NUM_CATEGORIES_TYPES = IDUM(1)

      ! Act on the Value of NUM_CATEGORIES_TYPES
      ! ------------------------

      ! Invalid Option
      IF (NUM_CATEGORIES_TYPES < MINCAT) THEN
         WRITE (MSG, 9001) NUM_CATEGORIES_TYPES, LINE
         CALL RAISE_ERROR(ERRLVL_fatal, 1, OUNIT, 0, 0, MSG)

         ! Special Case: Return to Caller
      ELSE IF (NUM_CATEGORIES_TYPES < 0) THEN
         RETURN

         ! No Categories
      ELSE IF (NUM_CATEGORIES_TYPES == 0) THEN
         ! Loop over output vectors
         DO I2 = 1, N2

            ! Get values for link elements
            IF (BLINK) THEN
               NEXT = LINE//'a'
               CALL ALREAD(3, IUNIT, OUNIT, NEXT(:LN), NLF, 1, IDUM0, CDUM, IDUM, AEL(1, I2))
            END IF

            ! Get values for grid elements ...
            NEXT = LINE//'b'
            CALL ALREAD(5, IUNIT, OUNIT, NEXT(:LN), NX, NY, IDUM0, CDUM, IDUM, DUMMY)

            ! ... and load into element array
            DO Y = 1, NY
               XY0 = (Y - 1)*NX
               DO X = 1, NX
                  IEL = ICMXY(X, Y)
                  IF (IEL > 0) AEL(IEL, I2) = DUMMY(XY0 + X)
               END DO
            END DO
         END DO

         ! Use category codes
      ELSE IF (N2*NUM_CATEGORIES_TYPES <= NELEE) THEN

         ! Get list of values for each category
         NEXT = LINE//'c'
         CALL ALREAD(3, IUNIT, OUNIT, NEXT(:LN), N2, NUM_CATEGORIES_TYPES, IDUM0, CDUM, IDUM, DUMMY)

         IF (NUM_CATEGORIES_TYPES == 1) THEN

            ! Uniform value: Set all elements or just columns
            N = NEL - FLAG*NLF
            I1 = 1 + NEL - N
            DO I2 = 1, N2
               ! Replaced ALINIT with Fortran array slice
               AEL(I1:I1 + N - 1, I2) = DUMMY(I2)
            END DO

         ELSE
            !
            ! Note: One code applies to all output vectors
            !
            ! Get codes & set values for link elements
            IF (BLINK) THEN
               NEXT = LINE//'d'

               ! Note: DUMMY should not be overwritten here
               CALL ALREAD(2, IUNIT, OUNIT, NEXT(:LN), NLF, 1, IDUM0, CDUM, IDUM, DUMMY)

               DO IEL = 1, NLF
                  ICAT = IDUM(IEL)

                  ! error if out of bounds
                  IF (ICAT < 1 .OR. ICAT > NUM_CATEGORIES_TYPES) THEN
                     WRITE (MSG, 9009) ICAT, NEXT(:LN), NUM_CATEGORIES_TYPES
                     CALL RAISE_ERROR(ERRLVL_fatal, 9, OUNIT, IEL, 0, MSG)
                  END IF

                  DO I2 = 1, N2
                     AEL(IEL, I2) = DUMMY(I2 + (ICAT - 1)*N2)
                  END DO
               END DO
            END IF

            ! Get codes & set values for grid elements
            NEXT = LINE//'e'
            CALL ALREAD(4, IUNIT, OUNIT, NEXT(:LN), NX, NY, NUM_CATEGORIES_TYPES, CDUM, IDUM, DUMMY)

            DO Y = 1, NY
               XY0 = (Y - 1)*NX
               DO X = 1, NX
                  IEL = ICMXY(X, Y)
                  IF (IEL > 0) THEN
                     ICAT = IDUM(XY0 + X)

                     ! error if out of bounds
                     IF (ICAT < 1 .OR. ICAT > NUM_CATEGORIES_TYPES) THEN
                        WRITE (MSG, 9009) ICAT, NEXT(:LN), NUM_CATEGORIES_TYPES
                        CALL RAISE_ERROR(ERRLVL_fatal, 9, OUNIT, IEL, 0, MSG)
                     END IF

                     DO I2 = 1, N2
                        AEL(IEL, I2) = DUMMY(I2 + (ICAT - 1)*N2)
                     END DO
                  END IF
               END DO
            END DO
         END IF

         ! Insufficient Workspace
      ELSE
         WRITE (MSG, 9008) NUM_CATEGORIES_TYPES, LINE, N2*NUM_CATEGORIES_TYPES
         CALL RAISE_ERROR(ERRLVL_fatal, 8, OUNIT, 0, 0, MSG)
      END IF
      !
      !
      ! Epilogue
      ! --------
      !
      ! All grid elements are defined - now set bank element values
      IF (NLF > 0 .AND. BEXBK .AND. NUM_CATEGORIES_TYPES /= 1) THEN
         DO I2 = 1, N2
            CALL ALBANK(NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF, AEL(NLF + 1, I2))
         END DO
      END IF

      RETURN

      ! Format Statements ----------------------------------------------------
9001  FORMAT('Invalid option NUM_CATEGORIES_TYPES =', I4, ' at title line ', A)

9008  FORMAT('Insufficient workspace for', I4, ' categories in ', A, &
             ' : increase NELEE to at least', I6)

9009  FORMAT('Invalid category value', I4, ' while reading ', A, &
             ' : should be in range [1,', I4, ']')

   END SUBROUTINE ALALLF

   !> Reads a distributed integer category field for column elements.
   !>
   !> The indexed grid under `LINE` is read by [[ALREDI]] and mapped through
   !> `ICMXY` into `CATTYP(NLF+1:NEL)`. Every active grid code must lie in
   !> `1:NUM_CATEGORIES_TYPES`; error 3090 is fatal otherwise. When `BEXBK` is
   !> true, each bank takes the category of the grid element across its outer
   !> face, falling back to the grid on the opposite side of the channel when
   !> that outer neighbour is absent.
   !>
   !> Unlike the stale legacy header, the implementation is not limited to
   !> nine categories. Counts below ten select the manual's compact `I1`
   !> integer-grid representation; larger counts use list-directed rows.
   !>
   !> @warning
   !> The bank-copy path assumes valid `ICMBK` references and at least one
   !> nonzero outer-grid reference for each link. It performs no topology or
   !> bounds check before indexing `CATTYP`.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | - | - | 4.2 or earlier | Created the integer distributed-category reader. |
   !> | 2025-10 | SB | 4.5.3 | Replaced a nitrate-specific invalid-category message with the current generic `ALALLI` diagnostic. |
   !> | 2026-04-06 | SvB | - | Replaced the legacy error jump with structured fatal-error handling. |
   !> @endhistory
   SUBROUTINE ALALLI(NUM_CATEGORIES_TYPES, IUNIT, OUNIT, LINE, NEL, NLF, NX, &
                     NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK, &
                     LINKNS, CATTYP, IDUM)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, ERRLVL_fatal, ERROR, ALREDI

      IMPLICIT NONE

      ! INPUT ARGUMENTS
      INTEGER(kind=I_P), INTENT(IN) :: NUM_CATEGORIES_TYPES !! Positive category count and integer-grid format selector.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Open input unit positioned before `LINE`.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving warnings and fatal diagnostics.
      INTEGER(kind=I_P), INTENT(IN) :: NEL !! Number of active elements, including links and banks.
      INTEGER(kind=I_P), INTENT(IN) :: NLF !! Number of active link elements excluded from `CATTYP`.
      INTEGER(kind=I_P), INTENT(IN) :: NX !! Active east-west grid extent.
      INTEGER(kind=I_P), INTENT(IN) :: NY !! Active north-south grid extent.
      INTEGER(kind=I_P), INTENT(IN) :: NELEE !! Element capacity extent.
      INTEGER(kind=I_P), INTENT(IN) :: NLFEE !! Link capacity extent.
      INTEGER(kind=I_P), INTENT(IN) :: NXEE !! Grid workspace extent in the x direction.
      INTEGER(kind=I_P), INTENT(IN) :: ICMXY(NXEE, NY) !! Active grid-coordinate to element-number map.
      INTEGER(kind=I_P), INTENT(IN) :: ICMBK(NLFEE, 2) !! Bank-element number for each link side.
      INTEGER(kind=I_P), INTENT(IN) :: ICMREF(NELEE, 4, 2:2) !! Outer adjacent-element references supplied from `AL_G:ICMREF(:,5:8)`.
      LOGICAL, INTENT(IN) :: BEXBK !! True when bank elements require copied categories.
      LOGICAL, INTENT(IN) :: LINKNS(NLFEE) !! True for north-south links; false for east-west links.
      CHARACTER(LEN=*), INTENT(IN) :: LINE !! Expected integer-grid heading substring.

      ! OUPUT ARGUMENTS
      INTEGER(kind=I_P), INTENT(OUT):: CATTYP(NLF + 1:NEL) !! Category by active grid/bank element; link elements are outside its bounds.

      ! WORKSPACE ARGUMENTS
      ! Changed to INTENT(INOUT) to fix compiler conflict with ALREDI modification
      INTEGER(kind=I_P), INTENT(INOUT) :: IDUM(*) !! Integer grid workspace; first `NX*NY` entries are overwritten by `ALREDI`.

      ! LOCALS ETC.
      INTEGER(kind=I_P) :: BANK1 !! Bank element on link side one.
      INTEGER(kind=I_P) :: BANK2 !! Bank element on link side two.
      INTEGER(kind=I_P) :: FACE1 !! Outer face number for `BANK1`.
      INTEGER(kind=I_P) :: FACE2 !! Outer face number for `BANK2`.
      INTEGER(kind=I_P) :: GRID1 !! Grid source for `BANK1`.
      INTEGER(kind=I_P) :: GRID2 !! Grid source for `BANK2`.
      INTEGER(kind=I_P) :: ISNS !! Orientation offset: one for a north-south link, zero otherwise.
      INTEGER(kind=I_P) :: LINK !! Active link index.
      INTEGER(kind=I_P) :: ICAT !! Category code at the current active grid coordinate.
      INTEGER(kind=I_P) :: IEL !! Element mapped from the current grid coordinate.
      INTEGER(kind=I_P) :: X !! Grid x index.
      INTEGER(kind=I_P) :: XY0 !! Offset of the current row in flattened `IDUM` storage.
      INTEGER(kind=I_P) :: Y !! Grid y index.

      ! Code =================================================================

      ! Read the category type for each element
      CALL ALREDI(NUM_CATEGORIES_TYPES, IUNIT, OUNIT, LINE, NX, NY, IDUM)

      DO Y = 1, NY
         XY0 = (Y - 1)*NX
         DO X = 1, NX
            IEL = ICMXY(X, Y)
            IF (IEL > 0) THEN
               ICAT = IDUM(XY0 + X)

               IF (ICAT < 1 .OR. ICAT > NUM_CATEGORIES_TYPES) THEN
                  CALL RAISE_ERROR(ERRLVL_fatal, 3090, OUNIT, 0, 0, &
                                   'Error in ALALLI -reading spatially distributed category types')
               END IF

               CATTYP(IEL) = ICAT
            END IF
         END DO
      END DO

      ! All grid elements are defined - now set bank element values
      ! Copied from ALBANK except an INTEGER(kind=I_P) array CATTYP is used
      ! instead of the floating point array.
      IF (NLF > 0 .AND. BEXBK) THEN
         ! Loop over channel links
         DO LINK = 1, NLF

            ! Determine orientation of link
            ISNS = 0
            IF (LINKNS(LINK)) ISNS = 1

            ! For each side of the channel: Determine adjacent bank element
            ! number, the number of its face that lies opposite to the
            ! channel, and the number of the grid element adjacent to
            ! that face.
            BANK1 = ICMBK(LINK, 1)
            BANK2 = ICMBK(LINK, 2)
            FACE1 = 2 - ISNS
            FACE2 = 4 - ISNS
            GRID1 = ICMREF(BANK1, FACE1, 2)
            GRID2 = ICMREF(BANK2, FACE2, 2)

            ! If the grid (as defined above) does not exist, then use
            ! the grid corresponding to the opposite side of the channel
            ! (precondition on ICMREF disallows GRID1 & GRID2 both zero)
            IF (GRID1 == 0) GRID1 = GRID2
            IF (GRID2 == 0) GRID2 = GRID1

            ! For each side of the channel, copy the contents of the array
            ! from the grid to its corresponding bank
            CATTYP(BANK1) = CATTYP(GRID1)
            CATTYP(BANK2) = CATTYP(GRID2)

         END DO
      END IF

   END SUBROUTINE ALALLI

   !> Copies adjacent grid values into the bank entries of an element array.
   !>
   !> For every active link, the link orientation selects the outer bank faces:
   !> north-south links use faces 1 and 3; east-west links use faces 2 and 4.
   !> `ICMREF(bank,face,2)` provides the grid element beyond each bank. If one
   !> side has no grid neighbour, the grid on the opposite side supplies both
   !> bank values. This implements the bank rule in User Manual Appendix A's
   !> floating-point element-array format and corresponds to legacy SSR51.
   !>
   !> @warning
   !> `ICMBK(link,1:2)` must contain valid bank elements and at least one of the
   !> two selected `ICMREF` values must be nonzero. If both are zero, the code
   !> indexes `A(0)`; the routine does not validate these entry conditions.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-04-22 | - | - | Initial version. |
   !> | 1994-05-23 | AB/RAH | 3.4.1 | Revised the bank-value propagation routine. |
   !> @endhistory
   SUBROUTINE ALBANK(NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF, A)

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: NEL !! Number of active elements and upper bound of `A`.
      INTEGER(kind=I_P), INTENT(IN) :: NLF !! Number of active channel links and lower-bound offset of `A`.
      INTEGER(kind=I_P), INTENT(IN) :: NLFEE !! Link capacity extent.
      INTEGER(kind=I_P), INTENT(IN) :: NELEE !! Element capacity extent.
      INTEGER(kind=I_P), INTENT(IN) :: ICMBK(NLFEE, 2) !! Bank-element number by link and side.
      INTEGER(kind=I_P), INTENT(IN) :: ICMREF(NELEE, 4, 2:2) !! Outer adjacent-element reference by element and face.
      LOGICAL, INTENT(IN) :: LINKNS(NLF) !! True for a north-south link; false for an east-west link.

      !
      ! Input/output arguments
      REAL(kind=R8P), INTENT(INOUT) :: A(NLF + 1:NEL) !! Element field whose two bank entries per link are overwritten.

      !
      ! Locals, etc
      INTEGER(kind=I_P) :: BANK1 !! Bank element on side one.
      INTEGER(kind=I_P) :: BANK2 !! Bank element on side two.
      INTEGER(kind=I_P) :: FACE1 !! Outer face of `BANK1`.
      INTEGER(kind=I_P) :: FACE2 !! Outer face of `BANK2`.
      INTEGER(kind=I_P) :: GRID1 !! Grid source for `BANK1`.
      INTEGER(kind=I_P) :: GRID2 !! Grid source for `BANK2`.
      INTEGER(kind=I_P) :: ISNS !! Orientation offset: one for north-south, zero for east-west.
      INTEGER(kind=I_P) :: LINK !! Active link index.

      ! Code =================================================================

      !
      ! Loop over channel links
      DO LINK = 1, NLF

         ! Determine orientation of link
         ISNS = 0
         IF (LINKNS(LINK)) ISNS = 1

         ! For each side of the channel: Determine adjacent bank element
         !  number, the number of it's face that lies opposite to the
         !  channel, and the number of the grid element adjacent to
         !  that face.
         BANK1 = ICMBK(LINK, 1)
         BANK2 = ICMBK(LINK, 2)
         FACE1 = 2 - ISNS
         FACE2 = 4 - ISNS
         GRID1 = ICMREF(BANK1, FACE1, 2)
         GRID2 = ICMREF(BANK2, FACE2, 2)

         ! If the grid ( as defined above ) does not exist, then use the
         ! grid corresponding to the opposite side of the channel
         ! ( precondition on ICMREF disallows GRID1 & GRID2 both zero )
         IF (GRID1 == 0) GRID1 = GRID2
         IF (GRID2 == 0) GRID2 = GRID1

         ! For each side of the channel, copy the contents of the array
         ! from the grid to its corresponding bank
         A(BANK1) = A(GRID1)
         A(BANK2) = A(GRID2)
         !
         ! Next channel link
      END DO

   END SUBROUTINE ALBANK

   !> Chooses the start and stride of an approximately even subsequence.
   !>
   !> For `M` selected positions among `N` candidate positions, the returned
   !> indices are `N1`, `N1+DEL`, ..., `N1+(M-1)*DEL`. The calculation centres
   !> the unused positions as evenly as its integer arithmetic permits and
   !> adjusts their parity when that produces a more uniform spread.
   !>
   !> The only current caller is [[vs_connectivity:VSCONC]], which uses these positions to
   !> distribute `M` foregone cell splits across `N` possible split locations.
   !> For `M=0`, `N1=N+1` selects no in-range position; for `M=1`, the one
   !> position is centred and `DEL=N`.
   !>
   !> @warning
   !> The intended domain is `0 <= M <= N` with positive `N`. It is not checked.
   !> Outside it the returned start/stride need not identify valid positions;
   !> `N=0` can return `DEL=0`, which is unsafe if a caller later uses it as a
   !> divisor or `MOD` argument.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1997-08-05 | RAH | 4.1 | Created the evenly spread subsequence calculation. |
   !> @endhistory
   SUBROUTINE ALSPRD(M, N, N1, DEL)

      ! Input arguments
      INTEGER(kind=I_P) :: M !! Number of positions to select.
      INTEGER(kind=I_P) :: N !! Number of available positions.

      ! Output arguments
      INTEGER(kind=I_P) :: N1 !! First selected one-based position.
      INTEGER(kind=I_P) :: DEL !! Integer stride between selected positions.

      ! Locals, etc
      INTEGER(kind=I_P) :: DNE !! Parity-preserving increment considered for the outlying count.
      INTEGER(kind=I_P) :: MM !! Number of intervals between selected positions (`M-1`).
      INTEGER(kind=I_P) :: NE !! Number of positions left outside the regular selected span.
      INTEGER(kind=I_P) :: NEMAX !! Maximum useful outlying-position count.
      INTEGER(kind=I_P) :: NF !! Alternative outlying count one interval above `NE`.

      LOGICAL :: TEST !! True when `NF` gives the preferred even/parity distribution.

      ! Code -----------------------------------------------------------------

      IF (M <= 1) THEN
         N1 = N/(MAX(0, M) + 1) + 1
         DEL = N

      ELSE
         ! set the number NE of out-lying items - even if possible
         MM = M - 1
         NE = MOD(N - 1, MM)
         NF = NE + MM
         TEST = (MOD(NE, 2) == 1) .AND. &
                (MOD(NF, 2) == 0) .AND. &
                (NF <= N - M)

         IF (TEST) NE = NF

         ! add a few if it makes a more uniform spread
         DNE = MM*(1 + MOD(MM, 2)*(1 - MOD(NE, 2)))
         NEMAX = 2*(N - M)/(M + 1)

         NE = NE + (IDIMJE(NEMAX, NE)/DNE)*DNE
         ! round up
         N1 = 1 + (NE + 1)/2

         DEL = (N - NE - 1)/MM
      END IF
   END SUBROUTINE ALSPRD

END MODULE spatial_fields

