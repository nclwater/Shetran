!> summary: Reads, expands, validates, and interpolates legacy SHETRAN input data.
!> author: AB / RAH, Newcastle University; JE, Newcastle University
!> author: Stephen Birkinshaw, Newcastle University; Sven Berendsen, Newcastle University
!>
!> This module implements the shared `AL*` input utilities used by the
!> sediment, contaminant, nitrate, and VSS components. It reads the generic
!> list, grid-array, floating-point element-array, and column element-array
!> formats defined in User Manual Appendix A; expands category or grid data to
!> element order; checks input relations; interpolates depth tables; and keeps
!> the legacy file-management and floating-point-trap entry points.
!>
!> The module is `PRIVATE` by default. Its 13 public procedures fall into these
!> groups:
!>
!> | Responsibility | Procedures |
!> |:---------------|:-----------|
!> | Distributed element data | [[ALALLF]], [[ALALLI]]; private helper [[ALBANK]] |
!> | Real and integer validation | [[ALCHK]], [[ALCHKI]] |
!> | Depth-table interpolation | [[ALINTP]] |
!> | Mixed legacy reader | [[ALREAD]] and its contained `throw_fatal` helper |
!> | Type-specific readers/file status | [[ALRED2]], [[ALREDC]], [[ALREDF]], [[ALREDI]], [[ALREDL]] |
!> | VSS spacing and startup compatibility | [[ALSPRD]], [[ALTRAP]] |
!>
!> Input headings are matched as case-sensitive substrings rather than exact
!> records. A mismatch is a warning and reading continues; missing or malformed
!> data are normally fatal through [[sglobal:ERROR]]. The readers retain
!> explicit-shape legacy interfaces, so some callers use valid Fortran sequence
!> association by passing an array element as the start of a contiguous data
!> sequence.
!>
!> @warning
!> `ALREAD` currently declares all three possible data destinations as
!> `INTENT(OUT)`. Standard Fortran therefore makes `CDATA`, `IDATA`, and
!> `RDATA` undefined on every call, including the two arrays not selected by
!> `FLAG`. In the multi-category path, [[ALALLF]] expects values previously
!> read into `DUMMY` to survive later integer-only `ALREAD` calls. That
!> expectation conflicts with the current interface and is documented here
!> without changing it.
!>
!> Only `HEAD0_alread` is maintained as a previous-heading diagnostic.
!> `HEAD0_alredc`, `HEAD0_alredi`, `HEAD0_alredf`, and `HEAD0_alredl` are read
!> on heading failures but never updated, so they retain their initial text;
!> `HEAD0_alred2` is updated but never read. These are current diagnostic-state
!> limitations, not automatic per-reader histories.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | - | AB/RAH | - | Created the original `AL*.F` input utilities. |
!> | 2012-08 | JE | - | Converted the utilities to Fortran 90 and combined them in this module. |
!> | 2020-03-05 | SvB | - | Reformatted the module, added documentation, and renamed selected category/table variables. |
!> | 2025-10 | SB | 4.5.3 | Generalized an `ALALLI` diagnostic, used active output extents in `ALINTP`, and expanded selected reader buffers. |
!> | 2026-04-05 | SvB | - | Removed `ALINIT`; its remaining use was replaced by an array-slice assignment. |
!> | 2026-04-06 | SvB | - | Replaced principal `GOTO` error/control paths with structured control flow and `IOSTAT` handling. |
!> | 2026-05-11 | SvB | - | Restored the public `ALTRAP` compatibility entry point during the current-code rebase. |
!> @endhistory
MODULE mod_load_filedata

   USE SGLOBAL
   USE mod_error, ONLY : RAISE_ERROR, ERRLVL_fatal, ERRLVL_warn, errstat_fileclose
   use mod_parameters

   IMPLICIT NONE

   CHARACTER(len=80) :: HEAD0_alread='( nothing read yet )' !! Most recent successful heading or status text processed by `ALREAD`.
   CHARACTER(len=80) :: HEAD0_alredc='( nothing read yet )' !! Fixed fallback text used by `ALREDC` heading-read errors; never updated.
   CHARACTER(len=80) :: HEAD0_alredi='( nothing read yet )' !! Fixed fallback text used by `ALREDI` heading-read errors; never updated.
   CHARACTER(len=80) :: HEAD0_alred2='( nothing read yet )' !! Most recent `ALRED2` status text; currently written but never read.
   CHARACTER(len=80) :: HEAD0_alredl='( nothing read yet )' !! Fixed fallback text used by `ALREDL` heading-read errors; never updated.
   CHARACTER(len=80) :: HEAD0_alredf='( nothing read yet )' !! Fixed fallback text used by `ALREDF` heading-read errors; never updated.


   ! --------------------------------------------------------------------------
   ! Private by default
   PRIVATE

   ! --------------------------------------------------------------------------
   ! Public methods
   PUBLIC :: ALREAD, ALALLF, ALCHKI, ALCHK, ALSPRD, ALTRAP,            &
      ALINTP, ALREDL, ALREDF, ALALLI, ALRED2, ALREDC, ALREDI


   ! Code =====================================================================

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
   SUBROUTINE ALALLF (FLAG, N2, MINCAT, IUNIT, OUNIT, LINE, NEL, NLF, NX, NY, NELEE, NLFEE, NXEE, &
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
      INTEGER(kind=I_P), INTENT(IN) :: ICMXY (NXEE, NY) !! Active grid-coordinate to element-number map.
      INTEGER(kind=I_P), INTENT(IN) :: ICMBK (NLFEE, 2) !! Bank-element number for each link side.
      INTEGER(kind=I_P), INTENT(IN) :: ICMREF (NELEE, 4, 2:2) !! East/north/west/south adjacent-element references supplied from `AL_G:ICMREF(:,5:8)`.
      LOGICAL, INTENT(IN) :: BEXBK !! True when explicit bank elements exist.
      LOGICAL, INTENT(IN) :: LINKNS (NLF) !! True for north-south links; false for east-west links.
      CHARACTER (LEN=*), INTENT(IN) :: LINE !! Base heading code; suffixed with `a` through `e` as required.

      ! Output arguments
      INTEGER(kind=I_P), INTENT(OUT)  :: NUM_CATEGORIES_TYPES !! Category count or permitted negative special-option value read from `LINE`.
      REAL(kind=R8P), INTENT(INOUT)   :: AEL (1 + NLF * (FLAG / N2) : NELEE - (NELEE - NEL) * (1 / N2), N2)
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
      LN = LEN (LINE) + 1
      BLINK = NLF > 0 .AND. FLAG == 0

      ! Find out how many categories ( if any )
      CALL ALREAD (2, IUNIT, OUNIT, LINE, 1, 1, IDUM0, CDUM, IDUM, DUMMY)
      NUM_CATEGORIES_TYPES = IDUM (1)


      ! Act on the Value of NUM_CATEGORIES_TYPES
      ! ------------------------

      ! Invalid Option
      IF (NUM_CATEGORIES_TYPES < MINCAT) THEN
         WRITE (MSG, 9001) NUM_CATEGORIES_TYPES, LINE
         CALL RAISE_ERROR (ERRLVL_fatal, 1, OUNIT, 0, 0, MSG)

      ! Special Case: Return to Caller
      ELSE IF (NUM_CATEGORIES_TYPES < 0) THEN
         RETURN

      ! No Categories
      ELSE IF (NUM_CATEGORIES_TYPES == 0) THEN
         ! Loop over output vectors
         DO I2 = 1, N2

            ! Get values for link elements
            IF (BLINK) THEN
               NEXT = LINE // 'a'
               CALL ALREAD (3, IUNIT, OUNIT, NEXT (:LN), NLF, 1, IDUM0, CDUM, IDUM, AEL (1, I2) )
            END IF

            ! Get values for grid elements ...
            NEXT = LINE // 'b'
            CALL ALREAD (5, IUNIT, OUNIT, NEXT (:LN), NX, NY, IDUM0, CDUM, IDUM, DUMMY)

            ! ... and load into element array
            DO Y = 1, NY
               XY0 = (Y - 1) * NX
               DO X = 1, NX
                  IEL = ICMXY (X, Y)
                  IF (IEL > 0) AEL (IEL, I2) = DUMMY (XY0 + X)
               END DO
            END DO
         END DO

      ! Use category codes
      ELSE IF (N2 * NUM_CATEGORIES_TYPES <= NELEE) THEN

         ! Get list of values for each category
         NEXT = LINE // 'c'
         CALL ALREAD (3, IUNIT, OUNIT, NEXT (:LN), N2, NUM_CATEGORIES_TYPES, IDUM0, CDUM, IDUM, DUMMY)

         IF (NUM_CATEGORIES_TYPES == 1) THEN

            ! Uniform value: Set all elements or just columns
            N = NEL - FLAG * NLF
            I1 = 1 + NEL - N
            DO I2 = 1, N2
               ! Replaced ALINIT with Fortran array slice
               AEL(I1 : I1 + N - 1, I2) = DUMMY(I2)
            END DO

         ELSE
            !
            ! Note: One code applies to all output vectors
            !
            ! Get codes & set values for link elements
            IF (BLINK) THEN
               NEXT = LINE // 'd'

               ! Note: DUMMY should not be overwritten here
               CALL ALREAD (2, IUNIT, OUNIT, NEXT (:LN), NLF, 1, IDUM0, CDUM, IDUM, DUMMY)

               DO IEL = 1, NLF
                  ICAT = IDUM (IEL)

                  ! error if out of bounds
                  IF (ICAT < 1 .OR. ICAT > NUM_CATEGORIES_TYPES) THEN
                     WRITE (MSG, 9009) ICAT, NEXT (:LN), NUM_CATEGORIES_TYPES
                     CALL RAISE_ERROR (ERRLVL_fatal, 9, OUNIT, IEL, 0, MSG)
                  END IF

                  DO I2 = 1, N2
                     AEL (IEL, I2) = DUMMY (I2 + (ICAT - 1) * N2)
                  END DO
               END DO
            END IF

            ! Get codes & set values for grid elements
            NEXT = LINE // 'e'
            CALL ALREAD (4, IUNIT, OUNIT, NEXT (:LN), NX, NY, NUM_CATEGORIES_TYPES, CDUM, IDUM, DUMMY)

            DO Y = 1, NY
               XY0 = (Y - 1) * NX
               DO X = 1, NX
                  IEL = ICMXY (X, Y)
                  IF (IEL > 0) THEN
                     ICAT = IDUM (XY0 + X)

                     ! error if out of bounds
                     IF (ICAT < 1 .OR. ICAT > NUM_CATEGORIES_TYPES) THEN
                        WRITE (MSG, 9009) ICAT, NEXT (:LN), NUM_CATEGORIES_TYPES
                        CALL RAISE_ERROR (ERRLVL_fatal, 9, OUNIT, IEL, 0, MSG)
                     END IF

                     DO I2 = 1, N2
                        AEL (IEL, I2) = DUMMY (I2 + (ICAT - 1) * N2)
                     END DO
                  END IF
               END DO
            END DO
         END IF

      ! Insufficient Workspace
      ELSE
         WRITE (MSG, 9008) NUM_CATEGORIES_TYPES, LINE, N2 * NUM_CATEGORIES_TYPES
         CALL RAISE_ERROR (ERRLVL_fatal, 8, OUNIT, 0, 0, MSG)
      END IF
      !
      !
      ! Epilogue
      ! --------
      !
      ! All grid elements are defined - now set bank element values
      IF (NLF > 0 .AND. BEXBK .AND. NUM_CATEGORIES_TYPES /= 1) THEN
         DO I2 = 1, N2
            CALL ALBANK (NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF, AEL (NLF + 1, I2) )
         END DO
      END IF

      RETURN

      ! Format Statements ----------------------------------------------------
9001  FORMAT ( 'Invalid option NUM_CATEGORIES_TYPES =', I4, ' at title line ', A )

9008  FORMAT ( 'Insufficient workspace for', I4, ' categories in ', A, &
         ' : increase NELEE to at least', I6 )

9009  FORMAT ( 'Invalid category value', I4, ' while reading ', A, &
         ' : should be in range [1,', I4, ']' )

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
   SUBROUTINE ALALLI (NUM_CATEGORIES_TYPES, IUNIT, OUNIT, LINE, NEL, NLF, NX,  &
                      NY, NELEE, NLFEE, NXEE, ICMXY, ICMBK, ICMREF, BEXBK,     &
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
      INTEGER(kind=I_P), INTENT(IN) :: ICMXY (NXEE, NY) !! Active grid-coordinate to element-number map.
      INTEGER(kind=I_P), INTENT(IN) :: ICMBK (NLFEE, 2) !! Bank-element number for each link side.
      INTEGER(kind=I_P), INTENT(IN) :: ICMREF (NELEE, 4, 2:2) !! Outer adjacent-element references supplied from `AL_G:ICMREF(:,5:8)`.
      LOGICAL, INTENT(IN) :: BEXBK !! True when bank elements require copied categories.
      LOGICAL, INTENT(IN) :: LINKNS (NLFEE) !! True for north-south links; false for east-west links.
      CHARACTER (LEN=*), INTENT(IN) :: LINE !! Expected integer-grid heading substring.

      ! OUPUT ARGUMENTS
      INTEGER(kind=I_P), INTENT(OUT):: CATTYP (NLF + 1:NEL) !! Category by active grid/bank element; link elements are outside its bounds.

      ! WORKSPACE ARGUMENTS
      ! Changed to INTENT(INOUT) to fix compiler conflict with ALREDI modification
      INTEGER(kind=I_P), INTENT(INOUT) :: IDUM (*) !! Integer grid workspace; first `NX*NY` entries are overwritten by `ALREDI`.

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
      CALL ALREDI (NUM_CATEGORIES_TYPES, IUNIT, OUNIT, LINE, NX, NY, IDUM)

      DO Y = 1, NY
         XY0 = (Y - 1) * NX
         DO X = 1, NX
            IEL = ICMXY (X, Y)
            IF (IEL > 0) THEN
               ICAT = IDUM (XY0 + X)

               IF (ICAT < 1 .OR. ICAT > NUM_CATEGORIES_TYPES) THEN
                  CALL RAISE_ERROR (ERRLVL_fatal, 3090, OUNIT, 0, 0, &
                              'Error in ALALLI -reading spatially distributed category types')
               END IF

               CATTYP (IEL) = ICAT
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
            IF (LINKNS (LINK)) ISNS = 1

            ! For each side of the channel: Determine adjacent bank element
            ! number, the number of its face that lies opposite to the
            ! channel, and the number of the grid element adjacent to
            ! that face.
            BANK1 = ICMBK (LINK, 1)
            BANK2 = ICMBK (LINK, 2)
            FACE1 = 2 - ISNS
            FACE2 = 4 - ISNS
            GRID1 = ICMREF (BANK1, FACE1, 2)
            GRID2 = ICMREF (BANK2, FACE2, 2)

            ! If the grid (as defined above) does not exist, then use
            ! the grid corresponding to the opposite side of the channel
            ! (precondition on ICMREF disallows GRID1 & GRID2 both zero)
            IF (GRID1 == 0) GRID1 = GRID2
            IF (GRID2 == 0) GRID2 = GRID1

            ! For each side of the channel, copy the contents of the array
            ! from the grid to its corresponding bank
            CATTYP (BANK1) = CATTYP (GRID1)
            CATTYP (BANK2) = CATTYP (GRID2)

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
   SUBROUTINE ALBANK (NEL, NLF, NLFEE, NELEE, ICMBK, LINKNS, ICMREF, A)

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: NEL !! Number of active elements and upper bound of `A`.
      INTEGER(kind=I_P), INTENT(IN) :: NLF !! Number of active channel links and lower-bound offset of `A`.
      INTEGER(kind=I_P), INTENT(IN) :: NLFEE !! Link capacity extent.
      INTEGER(kind=I_P), INTENT(IN) :: NELEE !! Element capacity extent.
      INTEGER(kind=I_P), INTENT(IN) :: ICMBK (NLFEE, 2) !! Bank-element number by link and side.
      INTEGER(kind=I_P), INTENT(IN) :: ICMREF (NELEE, 4, 2:2) !! Outer adjacent-element reference by element and face.
      LOGICAL, INTENT(IN) :: LINKNS (NLF) !! True for a north-south link; false for an east-west link.

      !
      ! Input/output arguments
      REAL(kind=R8P), INTENT(INOUT) :: A (NLF + 1:NEL) !! Element field whose two bank entries per link are overwritten.

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
         IF (LINKNS (LINK) ) ISNS = 1

         ! For each side of the channel: Determine adjacent bank element
         !  number, the number of it's face that lies opposite to the
         !  channel, and the number of the grid element adjacent to
         !  that face.
         BANK1 = ICMBK (LINK, 1)
         BANK2 = ICMBK (LINK, 2)
         FACE1 = 2 - ISNS
         FACE2 = 4 - ISNS
         GRID1 = ICMREF (BANK1, FACE1, 2)
         GRID2 = ICMREF (BANK2, FACE2, 2)

         ! If the grid ( as defined above ) does not exist, then use the
         ! grid corresponding to the opposite side of the channel
         ! ( precondition on ICMREF disallows GRID1 & GRID2 both zero )
         IF (GRID1 == 0) GRID1 = GRID2
         IF (GRID2 == 0) GRID2 = GRID1

         ! For each side of the channel, copy the contents of the array
         ! from the grid to its corresponding bank
         A (BANK1) = A (GRID1)
         A (BANK2) = A (GRID2)
         !
         ! Next channel link
      END DO

   END SUBROUTINE ALBANK


   !> Checks real values against a scalar or element-wise relation.
   !>
   !> `ALCHK` tests `SUBJ(N0:N1)` against either the scalar `OBJ(N0)` or the
   !> corresponding `OBJ(i)`. The last character `a` selects the array form;
   !> otherwise the object index remains `N0`. The first two operator
   !> characters select the relation:
   !>
   !> | `OP(1:2)` | Required relation | Failure test for finite values |
   !> |:----------|:------------------|:-------------------------------|
   !> | `LT` | `SUBJ < OBJ` | `SUBJ-OBJ >= TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !> | `GT` | `SUBJ > OBJ` | `OBJ-SUBJ >= TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !> | `LE` | `SUBJ <= OBJ` | `SUBJ-OBJ > TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !> | `GE` | `SUBJ >= OBJ` | `OBJ-SUBJ > TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !> | Any other second character | approximate equality | `ABS(SUBJ-OBJ) > TOL*MAX(ABS(SUBJ),ABS(OBJ))` |
   !>
   !> Every failure sets `NOTOK(i)` and increments cumulative `COUNT`. A
   !> negative `ACTION` first replaces each failing subject with its comparison
   !> value; `ABS(ACTION)` is then passed to [[sglobal:ERROR]] as the severity
   !> selector. The diagnostic gives the lowest-index failure and, for a
   !> nonfatal action, a continuation reports how many other entries failed.
   !> Up to three indices are inferred from commas in `SNAME`; `IX2` and `IX3`
   !> supply the fixed outer indices. This is the legacy SSR62 real checker.
   !>
   !> @warning
   !> `OP` must contain at least two characters and `TOL` is assumed finite and
   !> nonnegative. IEEE NaNs make every comparison used here false and can
   !> therefore pass validation; infinities can likewise produce unordered
   !> differences. No finite-value check is performed.
   !> @endwarning
   !>
   !> @note
   !> Legacy comments said [[ALCHKI]] was generated from this routine by a
   !> makefile. No such generator exists in the current build; the two source
   !> bodies are now maintained separately and should remain behaviorally
   !> aligned.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-07-22 | - | - | Initial version. |
   !> | 1994-08-17 | AB/RAH | 3.4.1 | Revised the relation checker. |
   !> | 2026-04-06 | SvB | - | Replaced the subscript-parser jump with a named-loop exit. |
   !> @endhistory
   SUBROUTINE ALCHK (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME, &
                     OP, OBJ, TOL, SUBJ, COUNT, NOTOK)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, R8P, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: ACTION !! Signed error severity; a negative value also resets failures.
      INTEGER(kind=I_P), INTENT(IN) :: ERRNUM !! Diagnostic code passed to `ERROR`.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Diagnostic output unit passed to `ERROR`.
      INTEGER(kind=I_P), INTENT(IN) :: N0 !! First checked vector index.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! Last checked vector index.
      INTEGER(kind=I_P), INTENT(IN) :: IX2 !! Fixed second subscript printed when `SNAME` implies two dimensions.
      INTEGER(kind=I_P), INTENT(IN) :: IX3 !! Fixed third subscript printed when `SNAME` implies three dimensions.
      CHARACTER(LEN=*), INTENT(IN) :: SNAME !! Display name whose comma syntax controls printed subscript count.
      CHARACTER(LEN=*), INTENT(IN) :: OP !! Two-character relation, optionally suffixed by `a` for an object array.
      REAL(kind=R8P), INTENT(IN) :: OBJ (N0: *) !! Scalar comparison at `N0` or element-wise comparison sequence.
      REAL(kind=R8P), INTENT(IN) :: TOL !! Relative tolerance used in every real relation.

      ! Input/output arguments
      REAL(kind=R8P), INTENT(INOUT) :: SUBJ (N0:N1) !! Values checked and, for negative `ACTION`, reset on failure.
      INTEGER(kind=I_P), INTENT(INOUT):: COUNT !! Cumulative failure count, incremented once per nonconforming value.

      ! Workspace arguments
      LOGICAL, INTENT(OUT) :: NOTOK (N0:N1) !! Per-value failure mask.

      ! Locals, etc
      INTEGER(kind=I_P) :: COUNT0 !! `COUNT` on entry.
      INTEGER(kind=I_P) :: COUNT1 !! Failures found by this call.
      INTEGER(kind=I_P) :: I !! Subject index.
      INTEGER(kind=I_P) :: INCOBJ !! Object-index increment: zero for scalar, one for element-wise comparison.
      INTEGER(kind=I_P) :: IOBJ !! Current object index.
      INTEGER(kind=I_P) :: IX (3) !! Indices printed for the lowest-index failure.
      INTEGER(kind=I_P) :: NDIM !! Number of printed indices inferred from `SNAME`, capped at three.
      INTEGER(kind=I_P) :: P !! Diagnostic index-list iterator.
      INTEGER(kind=I_P) :: POS1 !! Previous delimiter position while parsing `SNAME`.
      INTEGER(kind=I_P) :: POS2 !! Next parenthesis/comma position while parsing `SNAME`.
      INTEGER(kind=I_P) :: SGN !! Direction multiplier: one for less relations, minus one for greater relations.
      INTEGER(kind=I_P) :: SLEN !! Declared length of `SNAME`.
      REAL(kind=R8P) :: SB !! Subject value retained for the lowest-index failure.
      REAL(kind=R8P) :: OB !! Object value retained for the lowest-index failure.
      REAL(kind=R8P) :: rrr !! Diagnostic copy of `SB`, retained from the AD-oriented implementation.
      LOGICAL :: BRESET !! True when failures are replaced by object values.
      CHARACTER(len=9) :: CACT !! `Checking` or `Resetting` diagnostic verb.
      CHARACTER(len=132) :: MSG !! Error/continuation message buffer.
      CHARACTER :: OP1 !! First relation character; `G` reverses the comparison direction.
      CHARACTER :: OP2 !! Second relation character; `T`, `E`, or another character selects the test.

      ! Code =================================================================

      ! How many subscripts are there? (ignore any after the 3rd)
      ! ------------------------------
      SLEN = LEN (SNAME)
      POS1 = 0
      POS2 = INDEX (SNAME, '(')

      dim_loop: DO NDIM = 0, 2
         IF (POS2 > POS1 .AND. POS2 < SLEN) THEN
            IF (NDIM == 1) IX (2) = IX2
            IF (NDIM == 2) IX (3) = IX3
            POS1 = POS2
            POS2 = POS1 + INDEX (SNAME (POS1 + 1:), ',')
         ELSE
            EXIT dim_loop
         END IF
      END DO dim_loop

      ! If this point is traversed normally, NDIM=3; if exited early, NDIM<3

      ! What action is required?
      ! ------------------------
      BRESET = ACTION < 0
      OP1    = OP (1:1)
      OP2    = OP (2:2)
      SGN    = +1
      IF (OP1 == 'G') SGN = -1

      INCOBJ = 0
      IF (OP (LEN (OP) :) == 'a') INCOBJ = 1

      ! Store test results in logical workspace array
      ! ---------------------------------------------
      ! Note:  i Code is replicated to enable vectorization of loops.
      !       ii "Requirements" are approximate if TOL>0.

      IOBJ = N0

      IF (OP2 == 'T') THEN
         ! require SUBJ < OBJ or SUBJ > OBJ (depending on SGN)
         DO I = N0, N1
            SB        = SUBJ (I)
            OB        = OBJ (IOBJ)
            NOTOK (I) = SGN * (SB - OB) >= TOL * MAX (ABS (SB), ABS (OB))
            IOBJ      = IOBJ + INCOBJ
         END DO

      ELSE IF (OP2 == 'E') THEN
         ! require SUBJ <= OBJ or SUBJ >= OBJ (depending on SGN)
         DO I = N0, N1
            SB        = SUBJ (I)
            OB        = OBJ (IOBJ)
            NOTOK (I) = SGN * (SB - OB)  > TOL * MAX (ABS (SB), ABS (OB))
            IOBJ      = IOBJ + INCOBJ
         END DO

      ELSE
         ! require SUBJ == OBJ
         DO I = N0, N1
            SB        = SUBJ (I)
            OB        = OBJ (IOBJ)
            NOTOK (I) = ABS (SB - OB)  > TOL * MAX (ABS (SB), ABS (OB))
            IOBJ      = IOBJ + INCOBJ
         END DO
      END IF

      ! Count the non-conformances and fix them if required
      ! ---------------------------------------------------
      ! Note: Non-vectorizing loop: keep it short

      COUNT0 = COUNT
      IOBJ   = N0 + INCOBJ * (N1 - N0)

      ! step backwards so that IX(1), SB & OB refer to 1st non-conformer
      DO I = N1, N0, -1
         IF (NOTOK (I)) THEN
            COUNT  = COUNT + 1
            IX (1) = I
            SB     = SUBJ (I)
            OB     = OBJ (IOBJ)
            IF (BRESET) SUBJ (I) = OB
         END IF
         IOBJ = IOBJ - INCOBJ
      END DO

      ! Report findings
      ! ---------------
      COUNT1 = COUNT - COUNT0
      IF (COUNT1 > 0) THEN
         CACT = 'Checking'
         IF (BRESET) CACT = 'Resetting'

         ! print the first occurrence ...
         rrr = SB  !AD
         WRITE (MSG, 9000) CACT, SNAME, OP (:2), OB, rrr, (IX (P), P = 1, NDIM)
         CALL RAISE_ERROR (ABS (ACTION), ERRNUM, OUNIT, 0, 0, MSG)

         IF (COUNT1 > 1) THEN
            ! ... and allude to any others
            WRITE (MSG, 9010) COUNT1 - 1
            CALL RAISE_ERROR (0, 12, OUNIT, 0, 0, MSG)
         END IF
      END IF

      ! Format Statements ----------------------------------------------------
9000  FORMAT(A, 1X, A, ': expected .', A, '.', 1P, G15.7, ' but found', G15.7: &
             ' at position', I5, 2(:, ',', I4))
9010  FORMAT('... and similarly at', I4, &
             ' other positions in the same vector')

   END SUBROUTINE ALCHK


   !> Checks integer values against a scalar or element-wise relation.
   !>
   !> `ALCHKI` is the exact-integer counterpart of [[ALCHK]]. The optional
   !> trailing `a` in `OP` selects `OBJ(i)`; otherwise every subject is compared
   !> with `OBJ(N0)`.
   !>
   !> | `OP(1:2)` | Required relation | Failure test |
   !> |:----------|:------------------|:-------------|
   !> | `LT` | `SUBJ < OBJ` | `SUBJ-OBJ >= 0` |
   !> | `GT` | `SUBJ > OBJ` | `OBJ-SUBJ >= 0` |
   !> | `LE` | `SUBJ <= OBJ` | `SUBJ-OBJ > 0` |
   !> | `GE` | `SUBJ >= OBJ` | `OBJ-SUBJ > 0` |
   !> | Any other second character | equality | `ABS(SUBJ-OBJ) > 0` |
   !>
   !> Failure accounting, optional reset, severity selection, `SNAME` subscript
   !> parsing, and reporting are identical to `ALCHK`: `NOTOK` receives the
   !> mask, `COUNT` is cumulative, and a negative `ACTION` resets bad subjects
   !> before calling [[sglobal:ERROR]] with `ABS(ACTION)`.
   !>
   !> @warning
   !> `OP` must contain at least two characters. The subtraction, sign
   !> multiplication, and `ABS` operations use `INTEGER(kind=I_P)` without
   !> overflow checks, so extreme operands can invalidate the relation test.
   !> @endwarning
   !>
   !> @note
   !> The legacy makefile-generation warning is obsolete: no current build rule
   !> generates this routine from `ALCHK`; the two bodies are maintained by hand.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-07-22 | - | - | Initial version. |
   !> | 1994-08-17 | AB/RAH | 3.4.1 | Revised the integer relation checker. |
   !> | 2026-04-06 | SvB | - | Replaced the subscript-parser jump with a named-loop exit. |
   !> @endhistory
   SUBROUTINE ALCHKI (ACTION, ERRNUM, OUNIT, N0, N1, IX2, IX3, SNAME, &
                      OP, OBJ, SUBJ, COUNT, NOTOK)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: ACTION !! Signed error severity; a negative value also resets failures.
      INTEGER(kind=I_P), INTENT(IN) :: ERRNUM !! Diagnostic code passed to `ERROR`.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Diagnostic output unit passed to `ERROR`.
      INTEGER(kind=I_P), INTENT(IN) :: N0 !! First checked vector index.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! Last checked vector index.
      INTEGER(kind=I_P), INTENT(IN) :: IX2 !! Fixed second subscript printed for a two-dimensional display name.
      INTEGER(kind=I_P), INTENT(IN) :: IX3 !! Fixed third subscript printed for a three-dimensional display name.
      CHARACTER(LEN=*), INTENT(IN) :: SNAME !! Display name whose comma syntax controls printed subscript count.
      CHARACTER(LEN=*), INTENT(IN) :: OP !! Two-character relation, optionally suffixed by `a` for an object array.
      INTEGER(kind=I_P), INTENT(IN) :: OBJ (N0:*) !! Scalar comparison at `N0` or element-wise comparison sequence.

      ! Input/output arguments
      INTEGER(kind=I_P), INTENT(INOUT) :: SUBJ (N0:N1) !! Values checked and optionally reset.
      INTEGER(kind=I_P), INTENT(INOUT) :: COUNT !! Cumulative failure count.

      ! Workspace arguments
      LOGICAL, INTENT(OUT) :: NOTOK (N0:N1) !! Per-value failure mask.

      ! Locals, etc
      INTEGER(kind=I_P) :: COUNT0 !! `COUNT` on entry.
      INTEGER(kind=I_P) :: COUNT1 !! Failures found by this call.
      INTEGER(kind=I_P) :: I !! Subject index.
      INTEGER(kind=I_P) :: INCOBJ !! Object-index increment: zero for scalar, one for element-wise comparison.
      INTEGER(kind=I_P) :: IOBJ !! Current object index.
      INTEGER(kind=I_P) :: IX (3) !! Indices printed for the lowest-index failure.
      INTEGER(kind=I_P) :: NDIM !! Number of printed indices inferred from `SNAME`, capped at three.
      INTEGER(kind=I_P) :: P !! Diagnostic index-list iterator.
      INTEGER(kind=I_P) :: POS1 !! Previous delimiter position while parsing `SNAME`.
      INTEGER(kind=I_P) :: POS2 !! Next parenthesis/comma position while parsing `SNAME`.
      INTEGER(kind=I_P) :: SGN !! Direction multiplier: one for less relations, minus one for greater relations.
      INTEGER(kind=I_P) :: SLEN !! Declared length of `SNAME`.
      INTEGER(kind=I_P) :: SB !! Subject value retained for the lowest-index failure.
      INTEGER(kind=I_P) :: OB !! Object value retained for the lowest-index failure.
      INTEGER(kind=I_P) :: iii !! Diagnostic copy of `SB`, retained from the AD-oriented implementation.
      LOGICAL :: BRESET !! True when failures are replaced by object values.
      CHARACTER(len=9) :: CACT !! `Checking` or `Resetting` diagnostic verb.
      CHARACTER(len=132) :: MSG !! Error/continuation message buffer.
      CHARACTER :: OP1 !! First relation character; `G` reverses comparison direction.
      CHARACTER :: OP2 !! Second relation character; `T`, `E`, or another character selects the test.

      ! Code =================================================================

      ! How many subscripts are there? (ignore any after the 3rd)
      ! ------------------------------
      SLEN = LEN (SNAME)
      POS1 = 0
      POS2 = INDEX (SNAME, '(')

      dim_loop: DO NDIM = 0, 2
         IF (POS2 > POS1 .AND. POS2 < SLEN) THEN
            IF (NDIM == 1) IX (2) = IX2
            IF (NDIM == 2) IX (3) = IX3
            POS1 = POS2
            POS2 = POS1 + INDEX (SNAME (POS1 + 1:), ',')
         ELSE
            EXIT dim_loop
         END IF
      END DO dim_loop

      ! If this point is traversed normally, NDIM=3; if exited early, NDIM<3

      ! What action is required?
      ! ------------------------
      BRESET = ACTION < 0
      OP1    = OP (1:1)
      OP2    = OP (2:2)
      SGN    = +1
      IF (OP1 == 'G') SGN = -1

      INCOBJ = 0
      IF (OP (LEN (OP) :) == 'a') INCOBJ = 1

      ! Store test results in logical workspace array
      ! ---------------------------------------------
      ! Note:  i Code is replicated to enable vectorization of loops.

      IOBJ = N0

      IF (OP2 == 'T') THEN
         ! require SUBJ < OBJ or SUBJ > OBJ (depending on SGN)
         DO I = N0, N1
            SB        = SUBJ (I)
            OB        = OBJ (IOBJ)
            NOTOK (I) = SGN * (SB - OB) >= 0
            IOBJ      = IOBJ + INCOBJ
         END DO

      ELSE IF (OP2 == 'E') THEN
         ! require SUBJ <= OBJ or SUBJ >= OBJ (depending on SGN)
         DO I = N0, N1
            SB        = SUBJ (I)
            OB        = OBJ (IOBJ)
            NOTOK (I) = SGN * (SB - OB) > 0
            IOBJ      = IOBJ + INCOBJ
         END DO

      ELSE
         ! require SUBJ == OBJ
         DO I = N0, N1
            SB        = SUBJ (I)
            OB        = OBJ (IOBJ)
            NOTOK (I) = ABS (SB - OB) > 0
            IOBJ      = IOBJ + INCOBJ
         END DO
      END IF

      ! Count the non-conformances and fix them if required
      ! ---------------------------------------------------
      ! Note: Non-vectorizing loop: keep it short

      COUNT0 = COUNT
      IOBJ   = N0 + INCOBJ * (N1 - N0)

      ! step backwards so that IX(1), SB & OB refer to 1st non-conformer
      DO I = N1, N0, -1
         IF (NOTOK (I)) THEN
            COUNT  = COUNT + 1
            IX (1) = I
            SB     = SUBJ (I)
            OB     = OBJ (IOBJ)
            IF (BRESET) SUBJ (I) = OB
         END IF
         IOBJ = IOBJ - INCOBJ
      END DO

      ! Report findings
      ! ---------------
      COUNT1 = COUNT - COUNT0
      IF (COUNT1 > 0) THEN
         CACT = 'Checking'
         IF (BRESET) CACT = 'Resetting'

         ! print the first occurrence ...
         iii = SB !AD
         WRITE (MSG, 9000) CACT, SNAME, OP (:2), OB, iii, (IX (P), P = 1, NDIM)
         CALL RAISE_ERROR (ABS (ACTION), ERRNUM, OUNIT, 0, 0, MSG)

         IF (COUNT1 > 1) THEN
            ! ... and allude to any others
            WRITE (MSG, 9010) COUNT1 - 1
            CALL RAISE_ERROR (0, 12, OUNIT, 0, 0, MSG)
         END IF
      END IF

      ! Format Statements ----------------------------------------------------
9000  FORMAT(A, 1X, A, ': expected .', A, '.', I12, ' but found', I12: &
             ' at position', I5, 2(:, ',', I4))
9010  FORMAT('... and similarly at', I4, &
             ' other positions in the same vector')

   END SUBROUTINE ALCHKI


   !> Interpolates category-specific depth profiles onto active column cells.
   !>
   !> For each non-link element `NELM`, `NCATTY(NELM)` selects a table with
   !> `NTAB(category)` depth/value pairs. The top cell is assigned the first
   !> table value directly. For successively deeper cells down to
   !> `NCOLMB(NELM)`, the routine advances through the ordered table and uses
   !> linear interpolation:
   !>
   !> \[
   !> V(z)=V_{j-1}+(V_j-V_{j-1})
   !>       \frac{z-z_{j-1}}{z_j-z_{j-1}}.
   !> \]
   !>
   !> Here `z` is accumulated cell-centre depth from `DELTAZ` and `ZVSNOD`.
   !> Values at or below the final table depth are clamped to the final value.
   !> The value units are those of `TABLE_CONCENTRATION`; despite its legacy
   !> name this argument also carries nitrate process-parameter profiles in
   !> [[mnmod]]. [[frmod:INCM]] uses the routine for initial contaminant
   !> concentration profiles. Depths and vertical geometry are in metres.
   !>
   !> Only `CELL_CONCENTRATION(NLF+1:NEL,NCOLMB(element):NCETOP)` is defined.
   !> Link rows, cells below each active column, and any unused capacity are
   !> outside the result domain. This is the interpolation described for the
   !> contaminant and nitrate depth tables in the manual and legacy SSR51.
   !>
   !> @warning
   !> Category codes and table counts are used without bounds checks. Every
   !> active category needs a first entry at depth zero and strictly increasing
   !> depths. A column with cells below the top also needs at least two entries;
   !> otherwise `NTABLE` can identify a nonexistent second entry or the
   !> denominator can be zero. The routine assumes cell-centre depths increase
   !> monotonically while cell indices descend.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | - | - | - | Created the category depth-table interpolation routine. |
   !> | 2025-10 | SB | 4.5.3 | Changed the result extent from capacity bounds to active `NEL` and `NCETOP` bounds. |
   !> | 2026-04-06 | SvB | - | Replaced the interval-search jump with a named-loop exit. |
   !> @endhistory
   SUBROUTINE ALINTP (LLEE, NCETOP, NEL, NELEE, NLF, NUM_CATEGORIES_TYPES,     &
                      MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS, NCATTY,      &
                      NCOLMB, NTAB, TABLE_CONCENTRATION, TABLE_WATER_DEPTH,    &
                      DELTAZ, ZVSNOD, CELL_CONCENTRATION)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, R8P, two

      IMPLICIT NONE

      ! INPUT ARGUMENTS
      INTEGER(kind=I_P), INTENT(IN) :: LLEE !! Vertical-cell capacity extent of the geometry arrays.
      INTEGER(kind=I_P), INTENT(IN) :: NCETOP !! Top active VSS cell index and result second extent.
      INTEGER(kind=I_P), INTENT(IN) :: NEL !! Number of active elements and result first extent.
      INTEGER(kind=I_P), INTENT(IN) :: NELEE !! Element capacity extent of the geometry arrays.
      INTEGER(kind=I_P), INTENT(IN) :: NLF !! Number of link elements excluded from interpolation.
      INTEGER(kind=I_P), INTENT(IN) :: NUM_CATEGORIES_TYPES !! Number of active depth-profile categories.
      INTEGER(kind=I_P), INTENT(IN) :: MAX_NUM_CATEGORY_TYPES !! Allocated first extent of the table arrays.
      INTEGER(kind=I_P), INTENT(IN) :: MAX_NUM_DATA_PAIRS !! Allocated second extent of the table arrays.
      INTEGER(kind=I_P), INTENT(IN) :: NCATTY (NLF + 1:NEL) !! Profile category by non-link element.
      INTEGER(kind=I_P), INTENT(IN) :: NCOLMB (NLF + 1:NEL) !! Bottom active column-cell index by non-link element.
      INTEGER(kind=I_P), INTENT(IN) :: NTAB (NUM_CATEGORIES_TYPES) !! Active depth/value-pair count by category.

      REAL(kind=R8P), INTENT(IN) :: TABLE_CONCENTRATION (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS) !! Profile value by category and table entry.
      REAL(kind=R8P), INTENT(IN) :: TABLE_WATER_DEPTH (MAX_NUM_CATEGORY_TYPES, MAX_NUM_DATA_PAIRS) !! Depth below ground surface by category and entry (m).
      REAL(kind=R8P), INTENT(IN) :: DELTAZ (LLEE, NELEE) !! VSS cell thickness by cell and element (m).
      REAL(kind=R8P), INTENT(IN) :: ZVSNOD (LLEE, NELEE) !! VSS node elevation by cell and element (m).

      ! OUTPUT ARGUMENTS
      REAL(kind=R8P), INTENT(OUT) :: CELL_CONCENTRATION (NEL, NCETOP) !! Interpolated profile values; only active non-link cells are assigned.

      ! LOCALS ETC.
      INTEGER(kind=I_P) :: NCL !! Current VSS cell index.
      INTEGER(kind=I_P) :: NELM !! Current non-link element index.
      INTEGER(kind=I_P) :: NCATG !! Profile category selected for `NELM`.
      INTEGER(kind=I_P) :: NINTB !! Active table-entry count for `NCATG`.
      INTEGER(kind=I_P) :: NTABLE !! Upper bracketing table-entry index.
      INTEGER(kind=I_P) :: NTHRTB !! First candidate upper bracket for the next deeper cell.
      REAL(kind=R8P) :: DEPTH !! Accumulated current cell-centre depth below the surface (m).

      ! Code =================================================================

      element_loop: DO NELM = NLF + 1, NEL
         ! Category number for the element
         NCATG = NCATTY (NELM)

         ! Number of values in the table for this category number
         NINTB = NTAB (NCATG)

         ! The first depth in the table must be zero and the top
         ! cell is set to take the concentration at this depth
         CELL_CONCENTRATION (NELM, NCETOP) = TABLE_CONCENTRATION (NCATG, 1)
         DEPTH  = DELTAZ (NCETOP, NELM) / two
         NTHRTB = 2

         cell_loop: DO NCL = NCETOP - 1, NCOLMB (NELM), -1

            DEPTH = DEPTH + (ZVSNOD (NCL + 1, NELM) - ZVSNOD (NCL, NELM))

            ! The depth of the cell is greater than the lowest depth in
            ! the table and the cell takes the value of the concentration
            ! at the lowest specified depth
            IF (DEPTH >= TABLE_WATER_DEPTH (NCATG, NINTB)) THEN
               CELL_CONCENTRATION (NELM, NCL) = TABLE_CONCENTRATION (NCATG, NINTB)
               CYCLE cell_loop
            END IF

            ! Find the correct interval for interpolation
            search_loop: DO NTABLE = NTHRTB, NINTB
               IF (DEPTH <= TABLE_WATER_DEPTH (NCATG, NTABLE)) EXIT search_loop
               NTHRTB = NTHRTB + 1
            END DO search_loop

            ! Calculate concentration by linear interpolation
            CELL_CONCENTRATION (NELM, NCL) = &
               TABLE_CONCENTRATION (NCATG, NTABLE - 1) + &
               (TABLE_CONCENTRATION (NCATG, NTABLE) - TABLE_CONCENTRATION (NCATG, NTABLE - 1)) * &
               ((DEPTH - TABLE_WATER_DEPTH (NCATG, NTABLE - 1)) / &
               (TABLE_WATER_DEPTH (NCATG, NTABLE) - TABLE_WATER_DEPTH (NCATG, NTABLE - 1)))

         END DO cell_loop
      END DO element_loop

   END SUBROUTINE ALINTP




   !> Performs mixed file-status, character, integer, real, grid, and VSS input operations.
   !>
   !> For positive `FLAG`, `ALREAD` first reads an 80-character heading and
   !> checks whether it contains `LINE`. A mismatch raises warning 2 but does
   !> not prevent the selected data read. Zero and negative modes instead use
   !> `LINE` as the label in the file-status message.
   !>
   !> | `FLAG` | Operation | Principal dimensions/count |
   !> |:-------|:----------|:---------------------------|
   !> | `-1` | Close `IUNIT` and echo its status. | None. |
   !> | `0` | Require `IUNIT` to be open and echo its status. | None. |
   !> | `1` | Read one `(A)` character record into `CDATA`. | Character length of `CDATA`. |
   !> | `2` | Read a list-directed integer array. | `IDATA(N1,N2)`. |
   !> | `3` | Read a list-directed real array. | `RDATA(N1,N2)`. |
   !> | `4` | Read indexed integer-grid rows from `N2` down to 1. | `NUM_CATEGORIES_TYPES < 10` selects compact `I1`; otherwise list-directed input. |
   !> | `5` | Read indexed list-directed real-grid rows from `N2` down to 1. | Each record starts with the expected row number. |
   !> | `6` | Read indexed VSS item records. | Repeat `NUM_CATEGORIES_TYPES` times: item number, item count, integer values, then real values. |
   !> | `7` | Read VSS soil physical-property records. | For each category, three integers whose first value is the sequential ID, then eight reals. |
   !>
   !> Successful calls store the current heading or status in the private
   !> `HEAD0_alread`, which a later heading-read failure includes in its message.
   !> Error codes 3--7, 10--11, 14, and 16 distinguish heading, file/data,
   !> grid-row, soil, and VSS-record failures; contained helper `throw_fatal` dispatches
   !> each through [[sglobal:ERROR]] with fatal severity.
   !>
   !> @warning
   !> There is no `CASE DEFAULT`; unsupported flags may consume a heading and
   !> return without reading data, while unsupported negative flags can copy an
   !> undefined `HEAD` into `HEAD0_alread`. Modes 6 and 7 do not validate input
   !> item indices/counts against `N1` and `N2` before using them as subscripts.
   !>
   !> Because `CDATA`, `IDATA`, and `RDATA` all have `INTENT(OUT)`, standard
   !> Fortran makes every actual destination undefined on entry regardless of
   !> which `FLAG` is selected. Callers must not rely on either unused array
   !> retaining its previous value, although current [[ALALLF]] does so in its
   !> multi-category path.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial mixed-format reader. |
   !> | 1994-09-12 | GP | 4.0 | Added VSS modes 6 and 7. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy input reader. |
   !> | 1997-08-04 | RAH | 4.1 | Added end-of-file handling to modes 6 and 7 and renumbered the VSS error as 16. |
   !> | 2025-10-02 | SB | - | Increased the diagnostic message buffer from 132 to 140 characters. |
   !> | 2026-04-06 | SvB | - | Replaced error jumps with `SELECT CASE`, `IOSTAT`, and the contained fatal-error helper. |
   !> | 2026-09-06 | SvB | - | Checked the `CLOSE` through [[mod_error:errstat_fileclose]], reporting `IOSTAT`/`IOMSG`. |
   !> @endhistory
   SUBROUTINE ALREAD (FLAG, IUNIT, OUNIT, LINE, N1, N2, NUM_CATEGORIES_TYPES, &
                      CDATA, IDATA, RDATA)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, R8P, ERRLVL_warn, ERRLVL_fatal, HEAD0_alread, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: FLAG !! Operation selector from -1 through 7.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Input unit to inspect, close, or read.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving status output and diagnostics.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! First array extent or grid x extent.
      INTEGER(kind=I_P), INTENT(IN) :: N2 !! Second array extent or grid y extent.
      INTEGER(kind=I_P), INTENT(IN) :: NUM_CATEGORIES_TYPES !! Grid-code threshold or VSS record count, depending on `FLAG`.
      CHARACTER (LEN=*), INTENT(IN) :: LINE !! Expected heading substring or file-status label.

      ! Output arguments
      CHARACTER (LEN=*), INTENT(OUT) :: CDATA !! Character record returned by mode 1; undefined on entry for every mode.
      INTEGER(kind=I_P), INTENT(OUT) :: IDATA (N1, N2) !! Integer destination for modes 2, 4, 6, and 7; undefined on entry for every mode.
      REAL(kind=R8P), INTENT(OUT) :: RDATA (N1, N2) !! Real destination for modes 3, 5, 6, and 7; undefined on entry for every mode.

      ! Locals, etc
      CHARACTER (LEN=80) :: HEAD !! Heading read from input or formatted file-status text.
      CHARACTER (LEN=140) :: MSG !! Fatal/warning message buffer.
      CHARACTER (LEN=48) :: FILNAM !! Possibly truncated filename returned by `INQUIRE`.
      CHARACTER (LEN=17) :: FORM !! Generated compact integer-grid format.
      INTEGER(kind=I_P) :: IX !! Grid x or inner implied-DO index.
      INTEGER(kind=I_P) :: IY !! Expected indexed-grid row, processed north to south.
      INTEGER(kind=I_P) :: KY !! Row number read from an indexed-grid record.
      INTEGER(kind=I_P) :: IDUM1 !! VSS item index read by mode 6.
      INTEGER(kind=I_P) :: IDUM2 !! VSS item value count read by mode 6.
      INTEGER(kind=I_P) :: ICOUNT !! VSS/soil record iterator.
      INTEGER(kind=I_P) :: I !! Inner implied-DO index for VSS records.
      INTEGER(kind=I_P) :: ios !! I/O status from the most recent read or close.
      CHARACTER (LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from a failed close.
      LOGICAL :: BOPEN !! True when `IUNIT` is connected.
      LOGICAL :: BNAMED !! True when the connected unit has a filename.

      ! Code =================================================================

      !----------------------------------------------------------------------*
      ! Preliminaries
      ! -------------

      IF (FLAG > 0) THEN
         ! Check data header against what the caller expects to find
         READ (IUNIT, '(A)', IOSTAT=ios) HEAD

         IF (ios /= 0) THEN
            WRITE (MSG, 9801) LINE, HEAD0_alread
            CALL throw_fatal(3, MSG)
         END IF

         IF (INDEX (HEAD, LINE) == 0) THEN
            WRITE (MSG, 9002) LINE, HEAD
            CALL RAISE_ERROR (ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
         END IF

      ELSE
         ! Get file status and name
         INQUIRE (IUNIT, OPENED = BOPEN, NAMED = BNAMED, NAME = FILNAM)
         IF (.NOT. BNAMED) FILNAM = '(no name)'
      END IF

      ! Take Specified Action
      ! ---------------------
      SELECT CASE (FLAG)

      ! Check that input file is open
      CASE (0)
         IF (.NOT. BOPEN) THEN
            WRITE (MSG, 9000) LINE, 'not open', IUNIT
            CALL throw_fatal(4, MSG)
         END IF

         ! Write (and store) an informative message
         WRITE (HEAD, 9000) LINE, 'open', IUNIT, FILNAM
         WRITE (OUNIT, 9001) HEAD

      ! Close input file
      CASE (-1)
         CLOSE (IUNIT, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose (ios, TRIM(FILNAM), IUNIT, emsg)

         ! Write (and store) an informative message
         WRITE (HEAD, 9000) LINE, 'closed', IUNIT, FILNAM
         WRITE (OUNIT, 9001) HEAD

      ! Read a character string
      CASE (1)
         READ (IUNIT, '(A)', IOSTAT=ios) CDATA
         IF (ios /= 0) THEN
            WRITE (MSG, 9810) 'character', HEAD
            CALL throw_fatal(5, MSG)
         END IF

      ! Read an INTEGER(kind=I_P) array
      CASE (2)
         READ (IUNIT, *, IOSTAT=ios) IDATA
         IF (ios /= 0) THEN
            WRITE (MSG, 9810) 'integer', HEAD
            CALL throw_fatal(6, MSG)
         END IF

      ! Read a floating-point array
      CASE (3)
         READ (IUNIT, *, IOSTAT=ios) RDATA
         IF (ios /= 0) THEN
            WRITE (MSG, 9810) 'floating-point', HEAD
            CALL throw_fatal(7, MSG)
         END IF

      ! Read an INTEGER(kind=I_P) grid array
      CASE (4)
         ! Set format string to read single digit integers if possible
         IF (NUM_CATEGORIES_TYPES < 10) WRITE (FORM, 9410) N1

         ! All grid rows: North to South
         DO IY = N2, 1, -1
            IF (NUM_CATEGORIES_TYPES < 10) THEN
               READ (IUNIT, FORM, IOSTAT=ios) KY, (IDATA (IX, IY), IX = 1, N1)
            ELSE
               READ (IUNIT, *, IOSTAT=ios) KY, (IDATA (IX, IY), IX = 1, N1)
            END IF

            IF (ios /= 0 .OR. KY /= IY) THEN
               WRITE (MSG, 9842) 'integer', IY, HEAD
               CALL throw_fatal(10, MSG)
            END IF
         END DO

      ! Read a floating point grid array
      CASE (5)
         ! All grid rows: North to South
         DO IY = N2, 1, -1
            READ (IUNIT, *, IOSTAT=ios) KY, (RDATA (IX, IY), IX = 1, N1)
            IF (ios /= 0 .OR. KY /= IY) THEN
               WRITE (MSG, 9842) 'floating-point', IY, HEAD
               CALL throw_fatal(11, MSG)
            END IF
         END DO

      ! Read data in VSS format for each element
      CASE (6)
         DO ICOUNT = 1, NUM_CATEGORIES_TYPES
            READ (IUNIT, *, IOSTAT=ios) IDUM1, IDUM2
            IF (ios == 0) READ (IUNIT, *, IOSTAT=ios) (IDATA (IDUM1, I), I = 1, IDUM2)
            IF (ios == 0) READ (IUNIT, *, IOSTAT=ios) (RDATA (IDUM1, I), I = 1, IDUM2)

            IF (ios /= 0) THEN
               WRITE (MSG, 9600) IDUM1, HEAD
               CALL throw_fatal(16, MSG)
            END IF
         END DO

      ! Read soil physical property data for VSS
      CASE (7)
         DO ICOUNT = 1, NUM_CATEGORIES_TYPES
            READ (IUNIT, *, IOSTAT=ios) (IDATA (ICOUNT, I), I = 1, 3)
            IF (ios == 0 .AND. IDATA (ICOUNT, 1) == ICOUNT) THEN
               READ (IUNIT, *, IOSTAT=ios) (RDATA (ICOUNT, I), I = 1, 8)
            ELSE
               ! Trigger the error format if the IDs don't match
               ios = 1
            END IF

            IF (ios /= 0) THEN
               WRITE (MSG, 9700) ICOUNT, HEAD
               CALL throw_fatal(14, MSG)
            END IF
         END DO

      END SELECT

      ! Epilogue
      ! --------
      ! Store current title as old title
      HEAD0_alread = HEAD

      RETURN

      ! Format Statements ----------------------------------------------------

      ! -----------------
      ! Note: Take care not to exceed internal file length
9000  FORMAT ( A, ' data file ', A, ': unit', I3: '; ', A )
9001  FORMAT ( 1X, A/ )
9002  FORMAT ( 'Title line mismatch: expected "', A, &
               '" but found "', A, '"' )
9410  FORMAT ( '(I7,1X,', I4, 'I1)' )
9600  FORMAT ( 'Reading VSS data for item no. ', I4, ' under title: ', A )
9700  FORMAT ( 'Reading soils data for soil no. ', I4, ' under title: ', A )
9801  FORMAT ( 'Reading heading: ', A, '; last item was: ', A )
9810  FORMAT ( 'Reading ', A, ' data under heading: ', A )
9842  FORMAT ( 'Reading ', A, ' grid (IY=', I4, ') under title: ', A )


   CONTAINS

      !> Reports one `ALREAD` I/O failure as fatal.
      !>
      !> This contained helper host-associates `OUNIT` from [[ALREAD]] and calls
      !> [[sglobal:ERROR]] with `ERRLVL_fatal`, the supplied legacy error identifier,
      !> no element/cell context, and the already formatted message. `ERROR`
      !> terminates normal execution for fatal severity.
      !>
      !> @history
      !> | Date | Author | Version | Description |
      !> |:-----|:-------|:--------|:------------|
      !> | 2026-04-06 | SvB | - | Extracted repeated fatal-error dispatch while replacing `GOTO` paths. |
      !> @endhistory
      SUBROUTINE throw_fatal(err_id, err_msg)
         INTEGER(kind=I_P), INTENT(IN) :: err_id !! Legacy error code passed to `ERROR`.
         CHARACTER(LEN=*), INTENT(IN) :: err_msg !! Fully formatted diagnostic text.

         CALL RAISE_ERROR(ERRLVL_fatal, err_id, OUNIT, 0, 0, err_msg)
      END SUBROUTINE throw_fatal

   END SUBROUTINE ALREAD


   !> Checks or closes a legacy AL-family input file and echoes its status.
   !>
   !> `ALRED2` obtains the connection state and filename for `IUNIT`. With
   !> `FLAG=0` it requires the unit to be open; any other flag closes the unit.
   !> It writes a blank-line-terminated status message containing `LINE`, the
   !> operation, unit number, and filename to `OUNIT`, then stores that message
   !> in private `HEAD0_alred2`.
   !>
   !> The old header's statement that this routine contains `ENTRY` statements
   !> is obsolete. The 1995 refactor separated the character, real, integer,
   !> and logical operations into [[ALREDC]], [[ALREDF]], [[ALREDI]], and
   !> [[ALREDL]]; callers invoke those routines independently after this file
   !> lifecycle check.
   !>
   !> @note
   !> `HEAD0_alred2` has no current reader, so storing the status has no effect
   !> on later diagnostics. Its 80-character length also truncates the
   !> 152-character `HEAD`, as reported by gfortran. Closing is selected by
   !> every nonzero `FLAG`, not only by one distinguished value.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial input-file management routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Replaced the former `ENTRY` interface with separate type-specific `ALRED*` routines. |
   !> | 2025-10 | SB | - | Expanded the status, filename, and diagnostic buffers. |
   !> | 2026-04-06 | SvB | - | Replaced the file-not-open jump with structured error handling. |
   !> | 2026-09-06 | SvB | - | Checked the `CLOSE` through [[mod_error:errstat_fileclose]], reporting `IOSTAT`/`IOMSG`. |
   !> @endhistory
   SUBROUTINE ALRED2 (FLAG, IUNIT, OUNIT, LINE)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, ERRLVL_fatal, HEAD0_alred2, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: FLAG !! Zero checks the connection; any nonzero value closes it.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Input unit to inspect or close.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving the status message and fatal diagnostic.
      CHARACTER(LEN=*), INTENT(IN) :: LINE !! File-role label included in status text.

      ! Locals
      CHARACTER(152) :: HEAD !! Formatted open/closed status stored and written to `OUNIT`.
      CHARACTER(120) :: FILNAM !! Filename returned by `INQUIRE`, or `(no name)`.
      CHARACTER(200) :: MSG !! Fatal file-not-open message.
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! `IOMSG=` text from a failed close.
      INTEGER(kind=I_P) :: ios !! I/O status from the close.
      LOGICAL :: BOPEN !! True when `IUNIT` is connected.
      LOGICAL :: BNAMED !! True when the connected unit has an associated filename.

      ! Code -----------------------------------------------------------------

      !
      ! File Management
      ! ---------------
      !
      ! Get file status and name
      INQUIRE (IUNIT, OPENED = BOPEN, NAMED = BNAMED, NAME = FILNAM)
      IF (.NOT. BNAMED) FILNAM = '(no name)'

      IF (FLAG == 0) THEN

         ! Check that input file is open
         IF (.NOT. BOPEN) THEN
            WRITE (MSG, 9000) LINE, 'not open', IUNIT
            CALL RAISE_ERROR (ERRLVL_fatal, 4, OUNIT, 0, 0, MSG)
            RETURN
         END IF

         WRITE (HEAD, 9000) LINE, 'open', IUNIT, FILNAM

      ELSE
         ! Close input file
         CLOSE (IUNIT, IOSTAT=ios, IOMSG=emsg)
         CALL errstat_fileclose (ios, TRIM(FILNAM), IUNIT, emsg)
         WRITE (HEAD, 9000) LINE, 'closed', IUNIT, FILNAM
      END IF

      ! HEAD now contains an informative message
      WRITE (OUNIT, 9001) HEAD

      ! Store current title as old title
      HEAD0_alred2 = HEAD

      RETURN

      ! Formats --------------------------------------------------------------
9000  FORMAT (A, ' data file ', A, ': unit', I3: '; ', A)
9001  FORMAT (1X, A/)

   END SUBROUTINE ALRED2


   !> Reads a heading followed by fixed-format character records.
   !>
   !> `ALREDC` reads a heading into a 150-character buffer and tests whether it
   !> contains `LINE`. A mismatch raises warning 2 and input continues. It then
   !> reads `CDATA(N1,N2)` with format `(A)`; format reversion advances through
   !> successive records when more than one array item is requested. Current
   !> contaminant and nitrate callers request a single item.
   !>
   !> `FLAG` is retained for family-wide call compatibility but is not read.
   !> Heading and character-data failures are fatal errors 3 and 5. The heading
   !> error reports `HEAD0_alredc`, but that module field is never updated and
   !> therefore always says that nothing has yet been read.
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial character input routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Created the separate character reader during removal of the former `ENTRY` interface. |
   !> | 2025-10 | SB | - | Increased heading and message buffers to 150 and 200 characters. |
   !> @endhistory
   SUBROUTINE ALREDC (FLAG, IUNIT, OUNIT, LINE, N1, N2, CDATA)

      ! Input arguments
      INTEGER(kind=I_P) :: FLAG !! Unused selector retained for interface consistency.
      INTEGER(kind=I_P) :: IUNIT !! Open input unit positioned before the heading.
      INTEGER(kind=I_P) :: OUNIT !! Unit receiving warning/fatal diagnostics.
      INTEGER(kind=I_P) :: N1 !! First extent of `CDATA`.
      INTEGER(kind=I_P) :: N2 !! Second extent of `CDATA`.
      CHARACTER (LEN=*) :: LINE !! Expected case-sensitive heading substring.

      ! Output arguments
      CHARACTER(LEN=*) :: CDATA (N1, N2) !! Character records read in Fortran array element order.
      CHARACTER(len=150) :: HEAD !! Heading record read from `IUNIT`.
      CHARACTER(len=200) :: MSG !! Warning/fatal message buffer.

      ! Code -----------------------------------------------------------------

      READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD
      IF (INDEX (HEAD, LINE)  == 0) THEN
         WRITE (MSG, 9002) LINE, HEAD
         CALL RAISE_ERROR (ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
      ENDIF

      !  Read character data
      !  -------------------
      READ (IUNIT, '(A)', ERR = 8100, END = 8100) CDATA

      RETURN


      ! Errors ---------------------------------------------------------------

      ! Title line read error
8010  WRITE (MSG, 9801) LINE, HEAD0_alredc
      CALL RAISE_ERROR (ERRLVL_fatal, 3, OUNIT, 0, 0, MSG)

      ! Char data error
8100  WRITE (MSG, 9810) 'character', HEAD
      CALL RAISE_ERROR (ERRLVL_fatal, 5, OUNIT, 0, 0, MSG)


      ! Format ---------------------------------------------------------------

9002  FORMAT ( 'Title line mismatch: expected "', A,                          &
         '" but found "',                   A, '"' )

9801  FORMAT ( 'Reading heading: ', A, '; last item was: ', A )

9810  FORMAT ( 'Reading ', A, ' data under heading: ', A )

9842  FORMAT ( 'Reading ', A, ' grid (IY=',I4, ') under title: ', A )

   END SUBROUTINE ALREDC


   !> Reads a heading followed by a real list or indexed real grid.
   !>
   !> A heading containing `LINE` is expected first. A mismatch raises warning
   !> 2 but reading continues. `FLAG=0` reads all of `FDATA(N1,N2)` by
   !> list-directed input. Any nonzero flag reads `N2` indexed grid records in
   !> north-to-south order (`IY=N2,...,1`), each containing its row number and
   !> `N1` floating-point values. An I/O failure or wrong row number raises
   !> fatal error 11; heading and simple-array failures use errors 3 and 7.
   !>
   !> `HEAD0_alredf` is displayed on a heading-read failure but is never updated,
   !> so its text remains `( nothing read yet )` throughout the run.
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial real input routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Created the separate real reader and renamed the destination `FDATA`. |
   !> | 2026-04-06 | SvB | - | Replaced error jumps with `IOSTAT` checks and structured returns. |
   !> @endhistory
   SUBROUTINE ALREDF (FLAG, IUNIT, OUNIT, LINE, N1, N2, FDATA)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, R8P, ERRLVL_warn, ERRLVL_fatal, HEAD0_alredf, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: FLAG !! Zero selects a list; nonzero selects an indexed grid.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Open input unit positioned before the heading.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving warning/fatal diagnostics.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! First result extent or grid x extent.
      INTEGER(kind=I_P), INTENT(IN) :: N2 !! Second result extent or grid y extent.
      CHARACTER (LEN=*), INTENT(IN) :: LINE !! Expected case-sensitive heading substring.

      ! Output arguments
      REAL(kind=R8P), INTENT(OUT) :: FDATA (N1, N2) !! Real list or grid values read from `IUNIT`.

      ! Locals, etc
      INTEGER(kind=I_P) :: IY !! Expected grid row, processed from north to south.
      INTEGER(kind=I_P) :: KY !! Row number read from the current grid record.
      INTEGER(kind=I_P) :: IX !! Grid x index in the implied-DO input list.
      INTEGER(kind=I_P) :: ios !! I/O status from the latest read.
      CHARACTER(len=80) :: HEAD !! Heading record read from `IUNIT`.
      CHARACTER(len=132) :: MSG !! Warning/fatal message buffer.

      ! Code =================================================================

      READ (IUNIT, '(A)', IOSTAT=ios) HEAD

      IF (ios /= 0) THEN
         ! Title line read error
         WRITE (MSG, 9801) LINE, HEAD0_alredf
         CALL RAISE_ERROR (ERRLVL_fatal, 3, OUNIT, 0, 0, MSG)
         RETURN
      END IF

      IF (INDEX (HEAD, LINE) == 0) THEN
         WRITE (MSG, 9002) LINE, HEAD
         CALL RAISE_ERROR (ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
      END IF

      ! Read floating-point data
      ! ------------------------
      IF (FLAG == 0) THEN
         ! Simple array
         READ (IUNIT, *, IOSTAT=ios) FDATA

         IF (ios /= 0) THEN
            ! Real data error
            WRITE (MSG, 9810) 'floating-point', HEAD
            CALL RAISE_ERROR (ERRLVL_fatal, 7, OUNIT, 0, 0, MSG)
            RETURN
         END IF

      ELSE
         ! Grid-based array: read indexed rows, North to South
         DO IY = N2, 1, -1
            READ (IUNIT, *, IOSTAT=ios) KY, (FDATA (IX, IY), IX = 1, N1)

            IF (ios /= 0 .OR. KY /= IY) THEN
               ! Real grid error (or index mismatch)
               WRITE (MSG, 9842) 'floating-point', IY, HEAD
               CALL RAISE_ERROR (ERRLVL_fatal, 11, OUNIT, 0, 0, MSG)
               RETURN
            END IF
         END DO
      END IF

      RETURN

      ! Format ---------------------------------------------------------------
      !
      ! Note: Take care not to exceed internal file length
      !
9002  FORMAT ('Title line mismatch: expected "', A, '" but found "', A, '"')

9801  FORMAT ('Reading heading: ', A, '; last item was: ', A)

9810  FORMAT ('Reading ', A, ' data under heading: ', A)

9842  FORMAT ('Reading ', A, ' grid (IY=', I4, ') under title: ', A)

   END SUBROUTINE ALREDF


   !> Reads a heading followed by an integer list or indexed integer grid.
   !>
   !> A heading containing `LINE` is expected first; a mismatch raises warning
   !> 2 and reading continues. `FLAG=0` reads `IDATA(N1,N2)` by list-directed
   !> input. A nonzero flag reads indexed rows from `N2` down to 1. For flags
   !> below ten it generates format `(I7,1X,N1 I1)`, matching the manual's
   !> compact integer-grid form; flags of ten or more use list-directed rows.
   !> Every row number must equal the expected `IY`.
   !>
   !> Heading, list, and grid failures are fatal errors 3, 6, and 10.
   !> `HEAD0_alredi`, used in the heading error, is never updated and always
   !> retains its initial `( nothing read yet )` text.
   !>
   !> @warning
   !> Nonzero negative flags also enter the compact-grid branch because the
   !> code tests only `FLAG < 10`; callers are expected to pass a positive
   !> category limit for grid input.
   !> @endwarning
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial integer input routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Created the separate integer reader during removal of the former `ENTRY` interface. |
   !> | 2026-04-06 | SvB | - | Replaced error jumps with `IOSTAT` checks and structured returns. |
   !> @endhistory
   SUBROUTINE ALREDI (FLAG, IUNIT, OUNIT, LINE, N1, N2, IDATA)

      ! Assumed external module dependencies providing global kinds/variables:
      ! I_P, ERRLVL_warn, ERRLVL_fatal, HEAD0_alredi, ERROR

      IMPLICIT NONE

      ! Input arguments
      INTEGER(kind=I_P), INTENT(IN) :: FLAG !! Zero selects a list; nonzero selects a grid and also controls compact formatting.
      INTEGER(kind=I_P), INTENT(IN) :: IUNIT !! Open input unit positioned before the heading.
      INTEGER(kind=I_P), INTENT(IN) :: OUNIT !! Unit receiving warning/fatal diagnostics.
      INTEGER(kind=I_P), INTENT(IN) :: N1 !! First result extent or grid x extent.
      INTEGER(kind=I_P), INTENT(IN) :: N2 !! Second result extent or grid y extent.
      CHARACTER(LEN=*), INTENT(IN) :: LINE !! Expected case-sensitive heading substring.

      ! Output arguments
      INTEGER(kind=I_P), INTENT(OUT) :: IDATA (N1, N2) !! Integer list or grid values read from `IUNIT`.

      ! Locals, etc
      INTEGER(kind=I_P) :: IY !! Expected grid row, processed from north to south.
      INTEGER(kind=I_P) :: KY !! Row number read from the current grid record.
      INTEGER(kind=I_P) :: IX !! Grid x index in the implied-DO input list.
      INTEGER(kind=I_P) :: ios !! I/O status from the latest read.
      CHARACTER(len=80) :: HEAD !! Heading record read from `IUNIT`.
      CHARACTER(len=17) :: FORM !! Generated compact `I1` grid format.
      CHARACTER(len=132) :: MSG !! Warning/fatal message buffer.

      ! Code -----------------------------------------------------------------

      READ (IUNIT, '(A)', IOSTAT=ios) HEAD

      IF (ios /= 0) THEN
         ! Title line read error
         WRITE (MSG, 9801) LINE, HEAD0_alredi
         CALL RAISE_ERROR (ERRLVL_fatal, 3, OUNIT, 0, 0, MSG)
         RETURN
      END IF

      IF (INDEX (HEAD, LINE) == 0) THEN
         WRITE (MSG, 9002) LINE, HEAD
         CALL RAISE_ERROR (ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
      END IF

      ! Read INTEGER(kind=I_P) data
      ! -----------------
      IF (FLAG == 0) THEN
         ! Simple array
         READ (IUNIT, *, IOSTAT=ios) IDATA

         IF (ios /= 0) THEN
            ! Integer data error
            WRITE (MSG, 9810) 'integer', HEAD
            CALL RAISE_ERROR (ERRLVL_fatal, 6, OUNIT, 0, 0, MSG)
            RETURN
         END IF

      ELSE
         ! Grid-based array: read indexed rows, North to South
         ! (using single digit integers if possible)
         IF (FLAG < 10) WRITE (FORM, 9410) N1

         DO IY = N2, 1, -1
            IF (FLAG < 10) THEN
               READ (IUNIT, FORM, IOSTAT=ios) KY, (IDATA (IX, IY), IX = 1, N1)
            ELSE
               READ (IUNIT, *, IOSTAT=ios) KY, (IDATA (IX, IY), IX = 1, N1)
            END IF

            IF (ios /= 0 .OR. KY /= IY) THEN
               ! Integer grid error
               WRITE (MSG, 9842) 'integer', IY, HEAD
               CALL RAISE_ERROR (ERRLVL_fatal, 10, OUNIT, 0, 0, MSG)
               RETURN
            END IF
         END DO
      END IF

      RETURN

      ! Format ---------------------------------------------------------------
      !
      ! Note: Take care not to exceed internal file length
      !
9002  FORMAT ('Title line mismatch: expected "', A, '" but found "', A, '"')

9410  FORMAT ('(I7,1X,', I4, 'I1)')

9801  FORMAT ('Reading heading: ', A, '; last item was: ', A)

9810  FORMAT ('Reading ', A, ' data under heading: ', A)

9842  FORMAT ('Reading ', A, ' grid (IY=', I4, ') under title: ', A)

   END SUBROUTINE ALREDI


   !> Reads a heading followed by list-directed logical data.
   !>
   !> The routine checks the heading for the case-sensitive substring `LINE`,
   !> warning on a mismatch, then reads `LDATA(N1,N2)` by list-directed input.
   !> `FLAG` is accepted for interface compatibility but not used. Heading and
   !> logical-data failures invoke fatal errors 3 and 14.
   !>
   !> `HEAD0_alredl` is used by the heading-error message but never updated, so
   !> it retains the initial `( nothing read yet )` text.
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1993-12-10 | - | - | Initial logical input routine. |
   !> | 1994-09-16 | AB/RAH | 3.4.1 | Revised the legacy routine. |
   !> | 1995-03-22 | RAH | - | Added the logical reader while replacing the former `ENTRY` interface with separate routines. |
   !> @endhistory
   SUBROUTINE ALREDL (FLAG, IUNIT, OUNIT, LINE, N1, N2, LDATA)

      ! Input arguments
      INTEGER(kind=I_P) :: FLAG !! Unused selector retained for interface consistency.
      INTEGER(kind=I_P) :: IUNIT !! Open input unit positioned before the heading.
      INTEGER(kind=I_P) :: OUNIT !! Unit receiving warning/fatal diagnostics.
      INTEGER(kind=I_P) :: N1 !! First extent of `LDATA`.
      INTEGER(kind=I_P) :: N2 !! Second extent of `LDATA`.
      CHARACTER (LEN=*) :: LINE !! Expected case-sensitive heading substring.

      ! Output arguments
      LOGICAL :: LDATA (N1, N2) !! Logical values read in Fortran array element order.
      CHARACTER (80) :: HEAD !! Heading record read from `IUNIT`.
      CHARACTER(132) :: MSG !! Warning/fatal message buffer.

      ! Code -----------------------------------------------------------------

      READ (IUNIT, '(A)', ERR = 8010, END = 8010) HEAD
      IF (INDEX (HEAD, LINE)  == 0) THEN
         WRITE (MSG, 9002) LINE, HEAD
         CALL RAISE_ERROR (ERRLVL_warn, 2, OUNIT, 0, 0, MSG)
      ENDIF

      ! Read logical data
      ! -----------------
      READ (IUNIT, *, ERR = 8600, END = 8600) LDATA

      RETURN


      ! Error ----------------------------------------------------------------

      ! Title line read error
8010  WRITE (MSG, 9801) LINE, HEAD0_ALREDL
      CALL RAISE_ERROR(ERRLVL_fatal, 3, OUNIT, 0, 0, MSG)

      ! Logical data error
8600  WRITE (MSG, 9810) 'logical', HEAD
      CALL RAISE_ERROR(ERRLVL_fatal, 14, OUNIT, 0, 0, MSG)


      ! Format ---------------------------------------------------------------
      !
      ! Note: Take care not to exceed internal file length
      !
      !
9002  FORMAT ( 'Title line mismatch: expected "', A,                          &
         '" but found "',                   A, '"' )

9801  FORMAT ( 'Reading heading: ', A, '; last item was: ', A )

9810  FORMAT ( 'Reading ', A, ' data under heading: ', A )

   END SUBROUTINE ALREDL


   !> Chooses the start and stride of an approximately even subsequence.
   !>
   !> For `M` selected positions among `N` candidate positions, the returned
   !> indices are `N1`, `N1+DEL`, ..., `N1+(M-1)*DEL`. The calculation centres
   !> the unused positions as evenly as its integer arithmetic permits and
   !> adjusts their parity when that produces a more uniform spread.
   !>
   !> The only current caller is [[vsmod:VSCONC]], which uses these positions to
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
   SUBROUTINE ALSPRD (M, N, N1, DEL)

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
         N1  = N / (MAX (0, M) + 1) + 1
         DEL = N

      ELSE
         ! set the number NE of out-lying items - even if possible
         MM   = M - 1
         NE   = MOD (N - 1, MM)
         NF   = NE+MM
         TEST = (MOD(NE, 2) == 1)  .AND.                                     &
            (MOD(NF, 2)  == 0) .AND.                                     &
            (NF <= N - M)

         IF (TEST) NE = NF

         ! add a few if it makes a more uniform spread
         DNE   = MM * (1 + MOD (MM, 2) * (1 - MOD (NE, 2) ) )
         NEMAX = 2 * (N - M) / (M + 1)

         NE = NE+ (IDIMJE(NEMAX, NE) / DNE) * DNE
         ! round up
         N1 = 1 + (NE+1) / 2

         DEL = (N - NE-1) / MM
      ENDIF
   END SUBROUTINE ALSPRD


   !> Preserves the legacy startup hook for floating-point trap configuration.
   !>
   !> [[shetran]] calls `ALTRAP` once after command-line processing and before
   !> opening model files. The former platform-specific `IEEE_HANDLER` call has
   !> been commented out since version 4g-pc. The current routine sets its local
   !> status to zero and returns, so it enables no IEEE exceptions and can never
   !> issue its retained warning 13.
   !>
   !> @note
   !> The interface was removed as a no-op in April 2026 and restored during the
   !> May rebase because the main program still called it. Its presence must not
   !> be interpreted as active floating-point exception trapping.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-09-30 | RAH | 3.4.1 | Created the floating-point trap setup hook (legacy SSR79). |
   !> | 2000-03-07 | StevenB | 4g-pc | Removed the platform-specific IEEE handler calls. |
   !> | 2026-04-04 | SvB | - | Removed the no-op routine. |
   !> | 2026-05-11 | SvB | - | Restored the public no-op interface during the current-code rebase. |
   !> @endhistory
    SUBROUTINE ALTRAP ()

        ! Locals, etc
        INTEGER(kind=I_P), parameter :: OUT = 0 !! Retained diagnostic unit for the unreachable warning path.

        INTEGER(kind=I_P) :: I !! Legacy trap-setup status, unconditionally set to zero.

        ! Code -----------------------------------------------------------------

        !   I = IEEE_HANDLER( 'set', 'common', ABORT )
        I = 0
        IF (I .NE. 0) CALL RAISE_ERROR(ERRLVL_warn, 13, OUT, 0, 0,                         &
                           'Could not set traps for floating-point exceptions')

        RETURN
    END SUBROUTINE ALTRAP

END MODULE mod_load_filedata
