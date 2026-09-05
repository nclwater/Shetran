!> @brief Converts visualisation fields and topology into magnified map grids.
!>
!> This module expands every selected SHETRAN gridsquare into a `mag` by `mag`
!> raster block. The block interior represents the gridsquare, two-pixel strips
!> represent adjoining banks and river links, and zero represents background.
!> [[visualisation_hdf5]] is the only current consumer of the public API:
!>
!> | Public function | HDF5 product | Current magnification |
!> |:----------------|:-------------|:----------------------|
!> | [[get_real_image_index]] | Indexed `surf_elv` image under `/CATCHMENT_MAPS`. | 20 |
!> | [[get_magnified_su_arr]] | Element-number grid under `/CATCHMENT_SPREADSHEETS`. | 20 |
!>
!> Real compound data use nine members in the order produced for a `GS`
!> visualisation item. In the table, `x` is the first array dimension and `y`
!> is the second; low `y` is the north edge of the displayed grid.
!>
!> | Member | Element/value | Position within one `mag` by `mag` block |
!> |:------:|:--------------|:------------------------------------------|
!> | 1 | Gridsquare | Whole block initially; edge strips may overwrite it. |
!> | 2 | North bank | `x=3:mag-2`, `y=3:4` |
!> | 3 | East bank | `x=mag-3:mag-2`, `y=3:mag-2` |
!> | 4 | South bank | `x=3:mag-2`, `y=mag-3:mag-2` |
!> | 5 | West bank | `x=3:4`, `y=3:mag-2` |
!> | 6 | North river link | `x=3:mag-2`, `y=1:2` |
!> | 7 | East river link | `x=mag-1:mag`, `y=3:mag-2` |
!> | 8 | South river link | `x=3:mag-2`, `y=mag-1:mag` |
!> | 9 | West river link | `x=1:2`, `y=3:mag-2` |
!>
!> The metadata item's `on` mask and the display-oriented
!> [[visualisation_pass:SU_NUMBER]] grid select active source cells. Masked or
!> zero-numbered cells remain background. The topology-number product obtains
!> bank and river element numbers through `BANK_NO` and `RIVER_NO`; the indexed
!> real product instead receives those members in its input data.
!>
!> @warning
!> The hard-coded sections are not validated. `mag >= 6` is required for all
!> bank, river, and interior regions to be valid and visually distinct. `sz`
!> must contain at least two positive extents, match the metadata mask and
!> `SU_NUMBER` bounds, and match the second and third extents of `dat`; real
!> compound input must have exactly nine members. Both current callers use
!> `mag=20` and metadata-derived extents.
!> @endwarning
!>
!> @warning
!> Sentinel values overlap possible model data. Real zero is interpreted as
!> background, so an active surface at datum zero is omitted from scaling and
!> displayed as background. Exact `-1.0` in members 2:9 means no element data,
!> so a bank or link elevation of -1 m is not drawn. `HUGE(1.0)` marks rivers
!> before palette conversion.
!> @endwarning
!>
!> @warning
!> Palette scaling assumes at least two distinct nonzero, non-river values. A
!> uniform nonzero field gives `maxr==minr` and divides by zero. An empty mask
!> is accepted only incidentally because every output pixel then follows a
!> background/river branch that does not use the extrema.
!> @endwarning
!>
!> @warning
!> Private [[get_is_link_magnified]] is unused and unsafe with the normal
!> magnified grid: background pixels contain element zero, but `IS_LINK` is
!> allocated with lower bound one. Its vector subscript can therefore be out
!> of bounds. The function is retained as legacy code and is not on the public
!> call path.
!> @endwarning
!>
!> @note
!> Fortran applies the bare `PRIVATE` statement below to the whole module, so
!> only the two explicitly listed functions are public. The current FORD parser
!> applies default accessibility in source order and may label the preceding
!> parameters public in its generated table; their compiled accessibility is
!> private.
!> @endnote
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Added the visualisation map utilities to the repository. |
!> | 2026-03-29 | SvB | Replaced pointer results and temporaries with allocatables and made indexed-image assignment explicit. |
!> | 2026-04-04 | SvB | Applied the current source formatting. |
!> @endhistory
MODULE visualisation_map

   USE VISUALISATION_PASS, ONLY: BANK_NO, SU_NUMBER, RIVER_NO, north, east, south, west, IS_LINK
   USE VISUALISATION_METADATA, ONLY: G_L => GET_METADATA_L

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE MOD_ERROR, ONLY: errstat_alloc, errstat_dealloc

   IMPLICIT NONE

   INTEGER, PARAMETER :: mmax = 255       !! Highest palette slot; the current index generator does not emit it.
   INTEGER, PARAMETER :: i_background = 0 !! Palette index assigned to background and real zero values.
   INTEGER, PARAMETER :: i_river = mmax - 1 !! Palette index assigned to marked river-link pixels.
   REAL, PARAMETER    :: no_data = -1.0   !! Sentinel suppressing a bank or river member in real compound data.
   REAL, PARAMETER    :: background = 0.0 !! Real-grid sentinel converted to `i_background`.
   REAL, PARAMETER    :: river = HUGE(1.0) !! Real-grid sentinel converted to `i_river`.

   PRIVATE
   PUBLIC :: GET_REAL_IMAGE_INDEX, GET_MAGNIFIED_SU_ARR

CONTAINS

!> @brief Converts a real compound grid into indexed palette values.
!>
!> [[get_magnified_real]] first expands the selected source cells and marks
!> river strips. This function excludes real zero and the river sentinel from
!> the extrema, then maps ordinary values linearly from palette index 15 at
!> `minr` to 253 at `maxr`. Assignment to the integer result truncates any
!> fractional index. Background is index 0, rivers are index 254, and indices
!> 1:14 and 255 are not generated.
!>
!> The returned array has shape `(mag*sz(1), mag*sz(2))` and is owned by the
!> caller. The current caller is
!> [[visualisation_hdf5:save_surf_elev_as_map]], which passes the static
!> `surf_elv` compound grid with `mag=20`.
!>
!> @warning
!> Scaling requires `maxr > minr`. A uniform nonzero active field divides by
!> zero, and there is no finite-value validation. Real zero is always classified
!> as background, even when it is a valid active surface elevation.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Added indexed real-map generation. |
!> | 2026-03-29 | SvB | Changed pointer results to allocatables and replaced the masked `WHERE` assignment with explicit loops. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   FUNCTION get_real_image_index(sz, dat, mag, mn) RESULT(r)
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: r     !! Indexed magnified image returned to the caller.
      INTEGER, INTENT(IN)                  :: mag   !! Number of output pixels along each source-cell axis.
      INTEGER, INTENT(IN)                  :: mn    !! Metadata item index supplying the active-cell mask.
      INTEGER, DIMENSION(:), INTENT(IN)    :: sz    !! Source-grid `(x,y)` extents; at least two entries are required.
      REAL, DIMENSION(:, :, :), INTENT(IN)   :: dat   !! Nine compound values by source-grid `x` and `y`.
      REAL, DIMENSION(:, :), ALLOCATABLE    :: rreal !! Magnified real grid before conversion to palette indices.
      REAL                                 :: minr  !! Minimum nonzero, non-river value selected for scaling.
      REAL                                 :: maxr  !! Maximum nonzero, non-river value selected for scaling.
      INTEGER                              :: i     !! Magnified-grid first-dimension index.
      INTEGER                              :: j     !! Magnified-grid second-dimension index.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_MAP:get_real_image_index"

      rreal = GET_MAGNIFIED_REAL(sz, dat, mag, mn, mark_river=.TRUE.)
      minr = MINVAL(rreal, MASK=(rreal /= river .AND. rreal /= background))
      maxr = MAXVAL(rreal, MASK=(rreal /= river .AND. rreal /= background))

      ALLOCATE (r(mag*sz(1), mag*sz(2)), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "r", location, emsg)
      DO j = 1, mag*sz(2)
         DO i = 1, mag*sz(1)
            IF (rreal(i, j) == river) THEN
               r(i, j) = i_river
            ELSE IF (rreal(i, j) == background) THEN
               r(i, j) = i_background
            ELSE
               r(i, j) = 15 + (mmax - 17)*(rreal(i, j) - minr)/(maxr - minr)  !scaling
            END IF
         END DO
      END DO

      DEALLOCATE (rreal, STAT=ios, ERRMSG=emsg)
      CALL errstat_dealloc(ios, "rreal", location, emsg)
   END FUNCTION get_real_image_index

!> @brief Expands a real compound field into fixed-size raster blocks.
!>
!> The result is initialized to background zero. For every source coordinate
!> enabled by `G_L(mn,'on',i,j)`, a positive `SU_NUMBER(i,j)` selects one block
!> and [[get_dat_r]] supplies its gridsquare, bank, and river pixels. Masked and
!> zero-numbered coordinates leave their complete blocks at zero.
!>
!> `mark_river=.TRUE.` replaces members 6:9 with the private river sentinel;
!> false preserves their supplied values. The only current call uses true via
!> [[get_real_image_index]], so the false path is retained legacy behavior.
!>
!> @warning
!> `dat(:,i,j)` must conform to the explicit nine-element argument of
!> [[get_dat_r]], and `sz`, `dat`, the metadata mask, and `SU_NUMBER` must share
!> the same source-grid bounds. These preconditions are not checked.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Added masked real-field magnification. |
!> | 2026-03-29 | SvB | Changed the pointer result to an allocatable result. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   FUNCTION get_magnified_real(sz, dat, mag, mn, mark_river) RESULT(r)
      INTEGER, INTENT(IN)                 :: mag        !! Number of output pixels along each source-cell axis.
      INTEGER, INTENT(IN)                 :: mn         !! Metadata item index supplying the active-cell mask.
      INTEGER, DIMENSION(:), INTENT(IN)   :: sz         !! Source-grid `(x,y)` extents.
      REAL, DIMENSION(:, :, :), INTENT(IN)  :: dat        !! Nine compound values by source-grid `x` and `y`.
      LOGICAL, INTENT(IN)                 :: mark_river !! Whether river members become the private river sentinel.
      REAL, DIMENSION(:, :), ALLOCATABLE   :: r          !! Magnified real grid returned to the caller.
      INTEGER                             :: i          !! Source-grid first-dimension index.
      INTEGER                             :: j          !! Source-grid second-dimension index.
      INTEGER                             :: im         !! First-dimension offset of the current output block.
      INTEGER                             :: jm         !! Second-dimension offset of the current output block.
      INTEGER                             :: ilow       !! Fixed lower source first-dimension bound, one.
      INTEGER                             :: ihigh      !! Upper source first-dimension bound, `sz(1)`.
      INTEGER                             :: jlow       !! Fixed lower source second-dimension bound, one.
      INTEGER                             :: jhigh      !! Upper source second-dimension bound, `sz(2)`.
      INTEGER                             :: su         !! Display-oriented SHETRAN element number for the source cell.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_MAP:get_magnified_real"

      ALLOCATE (r(mag*sz(1), mag*sz(2)), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "r", location, emsg)

      ilow = 1
      ihigh = sz(1)
      jlow = 1
      jhigh = sz(2)
      im = -mag
      r = 0
      DO i = ilow, ihigh
         im = im + mag
         jm = -mag
         DO j = jlow, jhigh
            jm = jm + mag
            IF (.NOT. G_L(mn, 'on', i, j)) CYCLE
            su = SU_NUMBER(i, j)
            IF (su == 0) CYCLE  ! A non-model cell retains the background default.
            r(im + 1:im + mag, jm + 1:jm + mag) = GET_DAT_R(dat(:, i, j), su, mag, mark_river)
         END DO
      END DO

   END FUNCTION get_magnified_real

!> @brief Builds one real raster block from gridsquare, bank, and river values.
!>
!> The block starts at the gridsquare value `d9(1)`, with its one-pixel outer
!> border reset to background. Members 2:5 overwrite the inset bank strips and
!> members 6:9 overwrite the outer river strips according to the module layout
!> table. A member equal to `no_data` is skipped. When `mark_river` is true,
!> river strips receive the `river` sentinel instead of their source value.
!>
!> If `su==0`, the function returns after initializing the block and its border;
!> the current caller filters zero elements before calling it. A distinct full
!> layout requires `mag >= 6`.
!>
!> @warning
!> Exact real value `-1.0` means no data for every bank and river member, even
!> when -1 is a valid physical elevation. No tolerance or separate presence
!> mask is used.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Added compound real-block construction. |
!> @endhistory
   PURE FUNCTION get_dat_r(d9, su, mag, mark_river) RESULT(r)
      INTEGER, INTENT(IN)            :: su          !! Element number; zero requests only initialized defaults.
      INTEGER, INTENT(IN)            :: mag         !! Width and height of the returned raster block.
      REAL, DIMENSION(9), INTENT(IN) :: d9          !! Gridsquare, four bank, and four river values in layout order.
      LOGICAL, INTENT(IN)            :: mark_river  !! Whether river members are replaced by the river sentinel.
      REAL, DIMENSION(mag, mag)       :: r           !! Constructed real raster block.
      INTEGER                        :: b           !! Compound member being copied.
      INTEGER                        :: j           !! Retained legacy work index; unused.
      REAL                           :: dum         !! River value or sentinel written for members 6:9.
      r = d9(1)
      r(:, 1) = 0
      r(:, mag) = 0
      r(1, :) = 0
      r(mag, :) = 0
      IF (su == 0) RETURN
      DO b = 2, 9
         IF (d9(b) /= no_data) THEN
            IF (mark_river) THEN
               dum = river
            ELSE
               dum = d9(b)
            END IF
            SELECT CASE (b)
             CASE (2); r(3:mag - 2, 3:4) = d9(b)
             CASE (3); r(mag - 3:mag - 2, 3:mag - 2) = d9(b)
             CASE (4); r(3:mag - 2, mag - 3:mag - 2) = d9(b)
             CASE (5); r(3:4, 3:mag - 2) = d9(b)
             CASE (6); r(3:mag - 2, 1:2) = dum
             CASE (7); r(mag - 1:mag, 3:mag - 2) = dum
             CASE (8); r(3:mag - 2, mag - 1:mag) = dum
             CASE (9); r(1:2, 3:mag - 2) = dum
            END SELECT
         END IF
      END DO

   END FUNCTION get_dat_r

!> @brief Derives a logical river-link mask from a magnified element grid.
!>
!> This private legacy helper obtains the element-number raster from
!> [[get_magnified_su_arr]] and looks up each row in the `IS_LINK` table. It has
!> no current source-tree caller and is not exported.
!>
!> @warning
!> Magnified background and block-border pixels are zero, whereas `IS_LINK` is
!> allocated with lower bound one. The vector subscript `IS_LINK(su(i,:))` is
!> therefore out of bounds for normal input. Do not call this routine without
!> first changing how zero pixels are handled.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Added the private magnified link-mask helper. |
!> | 2026-03-29 | SvB | Changed its pointer result and temporary to allocatables. |
!> @endhistory
   FUNCTION get_is_link_magnified(sz, mag, mn) RESULT(r)
      INTEGER, INTENT(IN)                  :: mag !! Number of output pixels along each source-cell axis.
      INTEGER, INTENT(IN)                  :: mn  !! Metadata item index supplying the active-cell mask.
      INTEGER, DIMENSION(:), INTENT(IN)    :: sz  !! Source-grid `(x,y)` extents.
      LOGICAL, DIMENSION(:, :), ALLOCATABLE :: r   !! Logical magnified link mask returned to the caller.
      INTEGER                              :: i   !! Magnified-grid first-dimension index.
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: su  !! Magnified element-number grid, including background zeroes.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_MAP:get_is_link_magnified"

      su = GET_MAGNIFIED_SU_ARR(sz, mag, mn)
      ALLOCATE (r(mag*sz(1), mag*sz(2)), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "r", location, emsg)
      DO i = 1, mag*sz(1)
         r(i, :) = IS_LINK(su(i, :))
      END DO
      DEALLOCATE (su, STAT=ios, ERRMSG=emsg)
      CALL errstat_dealloc(ios, "su", location, emsg)
   END FUNCTION get_is_link_magnified

!> @brief Expands selected element numbers and topology into a raster grid.
!>
!> The result is initialized to zero. Each metadata-enabled source coordinate
!> with a positive `SU_NUMBER` receives a block from [[get_el]]: the gridsquare
!> number fills its interior, adjoining bank and river numbers occupy their
!> strips, and absent topology or block borders remain zero. Masked and
!> non-model source cells retain an all-zero block.
!>
!> This is the public integer-map entry point. The current HDF5 caller is
!> [[visualisation_hdf5:save_numbers_as_spreadsheet]], which requests `mag=20`
!> for the static `number` item.
!>
!> @warning
!> `mag`, `sz`, metadata-mask bounds, `SU_NUMBER`, and the topology tables are
!> assumed valid and initialized. There is no bounds or allocation check.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Added masked element-number magnification. |
!> | 2026-03-29 | SvB | Changed the pointer result to an allocatable result. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   FUNCTION get_magnified_su_arr(sz, mag, mn) RESULT(r)
      INTEGER, INTENT(IN)                  :: mag   !! Number of output pixels along each source-cell axis.
      INTEGER, INTENT(IN)                  :: mn    !! Metadata item index supplying the active-cell mask.
      INTEGER, DIMENSION(:), INTENT(IN)    :: sz    !! Source-grid `(x,y)` extents.
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: r     !! Magnified element-number grid returned to the caller.
      INTEGER, DIMENSION(mag, mag)          :: el    !! Retained legacy block workspace; unused.
      INTEGER                              :: i     !! Source-grid first-dimension index.
      INTEGER                              :: j     !! Source-grid second-dimension index.
      INTEGER                              :: im    !! First-dimension offset of the current output block.
      INTEGER                              :: jm    !! Second-dimension offset of the current output block.
      INTEGER                              :: ilow  !! Fixed lower source first-dimension bound, one.
      INTEGER                              :: ihigh !! Upper source first-dimension bound, `sz(1)`.
      INTEGER                              :: jlow  !! Fixed lower source second-dimension bound, one.
      INTEGER                              :: jhigh !! Upper source second-dimension bound, `sz(2)`.
      INTEGER                              :: su    !! Display-oriented SHETRAN gridsquare element number.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = "VISUALISATION_MAP:get_magnified_su_arr"

      ALLOCATE (r(mag*sz(1), mag*sz(2)), STAT=ios, ERRMSG=emsg)
      CALL errstat_alloc(ios, "r", location, emsg)
      ilow = 1
      ihigh = sz(1)
      jlow = 1
      jhigh = sz(2)
      im = -mag
      r = 0
      DO i = ilow, ihigh
         im = im + mag
         jm = -mag
         DO j = jlow, jhigh
            jm = jm + mag
            IF (.NOT. G_L(mn, 'on', i, j)) CYCLE
            su = SU_NUMBER(i, j)
            IF (su == 0) CYCLE  ! A non-model cell retains the background default.
            r(im + 1:im + mag, jm + 1:jm + mag) = GET_EL(su, mag)
         END DO
      END DO
   END FUNCTION get_magnified_su_arr

!> @brief Builds one magnified element-number block from topology tables.
!>
!> The block is initialized to the gridsquare number `su`, then its one-pixel
!> outer border is reset to background zero. Positive adjoining river and bank
!> element numbers overwrite the two-pixel strips shown in the module layout
!> table. Missing topology leaves the underlying gridsquare or border value
!> unchanged. `su==0` returns an all-zero block.
!>
!> `BANK_NO` and `RIVER_NO` retain native E,N,W,S columns, while the imported
!> `north`, `east`, `south`, and `west` constants select the correct columns for
!> the display positions. The normal caller supplies a positive gridsquare from
!> `SU_NUMBER`; other element kinds and out-of-range numbers are not checked.
!>
!> @warning
!> The fixed strips require `mag >= 6` for a distinct valid layout. No minimum
!> magnification or topology-table bound is enforced here.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Added topology-aware element-number block construction. |
!> @endhistory
   PURE FUNCTION get_el(su, mag) RESULT(r)
      INTEGER, INTENT(IN)         :: su  !! Gridsquare element number; zero requests a background block.
      INTEGER, INTENT(IN)         :: mag !! Width and height of the returned raster block.
      INTEGER, DIMENSION(mag, mag) :: r   !! Constructed element-number raster block.
      INTEGER                     :: j   !! Adjoining bank or river element number.
      r = su
      ! Establish background borders before overlaying present links and banks.
      r(:, 1) = 0
      r(:, mag) = 0
      r(1, :) = 0
      r(mag, :) = 0
      IF (su == 0) RETURN
      j = RIVER_NO(su, north); IF (j > 0) r(3:mag - 2, 1:2) = j
      j = BANK_NO(su, north); IF (j > 0) r(3:mag - 2, 3:4) = j
      j = BANK_NO(su, south); IF (j > 0) r(3:mag - 2, mag - 3:mag - 2) = j
      j = RIVER_NO(su, south); IF (j > 0) r(3:mag - 2, mag - 1:mag) = j
      j = RIVER_NO(su, west); IF (j > 0) r(1:2, 3:mag - 2) = j
      j = BANK_NO(su, west); IF (j > 0) r(3:4, 3:mag - 2) = j
      j = BANK_NO(su, east); IF (j > 0) r(mag - 3:mag - 2, 3:mag - 2) = j
      j = RIVER_NO(su, east); IF (j > 0) r(mag - 1:mag, 3:mag - 2) = j
   END FUNCTION get_el
END MODULE visualisation_map
