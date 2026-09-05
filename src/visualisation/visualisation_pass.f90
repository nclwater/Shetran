!> @brief Owns persistent startup state copied into the visualisation subsystem.
!>
!> [[visualisation_interface_right:send_pass]] is the sole current producer. It
!> calls [[send_p]] in two ordered passes, and this module retains the copied
!> scalars, filenames, classification masks, display grid, and topology tables
!> for the lifetime of the process.
!>
!> | Startup pass | Exact `SEND_P` keys | State established |
!> |:-------------|:--------------------|:------------------|
!> | 1 | `dirqq`, `rootdir`, `ver`, `hdf5fname`, `planfile`, `checkfile` | Directories, filenames, and integer major version. |
!> | 2, directions | `north`, `east`, `south`, `west` | Native face-column numbers. |
!> | 2, sizes | `grid_nx`, `grid_ny`, `top_cell`, `nel`, `nsed`, `ncon` | Active model dimensions and counts. |
!> | 2, classes | `is_square`, `is_bank`, `is_link` | Element-class masks. |
!> | 2, topology | `su`, `bank_no`, `river_no` | Display numbering and square-to-bank/link tables. |
!>
!> | Consumer | Imported state |
!> |:---------|:---------------|
!> | [[visualisation_metadata]] | Plan/check paths, dimensions/counts, element classes, display numbering, and topology. |
!> | [[visualisation_map]] | Display numbering, native face constants, link classification, and square topology. |
!> | [[visualisation_hdf5]] | HDF5 filename and major version; run/root paths are retained unused imports. |
!>
!> `SU_NUMBER(grid_nx,grid_ny)` uses the stored HDF5/SHEGRAPH orientation:
!> dimension one is display x, dimension two is display y, and zero denotes an
!> inactive grid position. `BANK_NO(nel,4)` and `RIVER_NO(nel,4)` retain native
!> SHETRAN face columns east=1, north=2, west=3, south=4. Their non-square rows
!> are zero. The separately transferred direction values let display code select
!> those native columns in north/east/south/west order.
!>
!> @warning
!> Phase 2 is one-shot. The six allocatable arrays are allocated without an
!> `ALLOCATED` check or cleanup path, and the remaining state is not initialized
!> locally. Consumers must run only after the matching keys have been sent, and
!> a second initialization in the same process is unsupported.
!> @endwarning
!>
!> @warning
!> [[send_p]] trusts its key, optional payload, declared dimensions, and call
!> order. It has no `PRESENT`, shape, allocation-status, or lower-bound checks.
!> An invalid contract can reference an absent optional argument, assign
!> nonconforming arrays, or use dimensions/counts before they are defined.
!> @endwarning
!>
!> @note
!> Fortran applies the bare `PRIVATE` statement below to the complete module.
!> Only the names in the explicit public list form the compiled API. The current
!> FORD parser applies default accessibility in source order and may label the
!> preceding private guard/work variables as public in the generated table.
!> @endnote
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2008-01-23 | Unknown | - | Added the dated SHEGRAPH DLL grid-dimension guard retained in `SEND_P`. |
!> | 2020-09-08 | SB | - | Imported the visualisation sources and removed the external SHEGRAPH DLL. |
!> | 2026-03-29 | SvB | - | Removed DEC conditional/export directives during the portability update. |
!> | 2026-04-04 | SvB | - | Applied project-wide indentation and line-length formatting. |
!> | 2026-04-08 | SB | 4.6.1 | Carried the portable pass-through implementation into the IFX visualisation update. |
!> @endhistory
MODULE visualisation_pass

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, I_P
   USE MOD_ERROR, ONLY: errstat_alloc

   IMPLICIT NONE

   INTEGER :: north    !! Native SHETRAN north-face column, currently 2.
   INTEGER :: east     !! Native SHETRAN east-face column, currently 1.
   INTEGER :: south    !! Native SHETRAN south-face column, currently 4.
   INTEGER :: west     !! Native SHETRAN west-face column, currently 3.
   INTEGER :: grid_nx  !! Number of columns in the stored display grid.
   INTEGER :: grid_ny  !! Number of rows in the stored display grid.
   INTEGER :: top_cell !! Number of active subsurface cells in a model column.
   INTEGER :: nel      !! Number of active SHETRAN elements; array indices are `1:nel`.
   INTEGER :: nsed     !! Number of sediment fractions available to visualisation metadata.
   INTEGER :: ncon     !! Number of contaminants available to visualisation metadata.
   INTEGER :: ver      !! Integer SHETRAN major version used in HDF5 dataset names.

   INTEGER, DIMENSION(:, :), ALLOCATABLE :: SU_NUMBER !! Display-grid SHETRAN element number; zero means inactive.
   INTEGER, DIMENSION(:, :), ALLOCATABLE :: BANK_NO   !! Bank element by square element and native face column.
   INTEGER, DIMENSION(:, :), ALLOCATABLE :: RIVER_NO  !! River-link element by square element and native face column.
   LOGICAL, DIMENSION(:), ALLOCATABLE   :: IS_SQUARE !! Element-class mask over `1:nel` for gridsquares.
   LOGICAL, DIMENSION(:), ALLOCATABLE   :: IS_BANK   !! Element-class mask over `1:nel` for bank elements.
   LOGICAL, DIMENSION(:), ALLOCATABLE   :: IS_LINK   !! Element-class mask over `1:nel` for river links.

   CHARACTER(256) :: DIRQQ        !! Run directory used to resolve the visualisation plan.
   CHARACTER(256) :: ROOTDIR      !! Compatibility root directory; imported but unused by the current HDF5 writer.
   CHARACTER(256) :: hdf5filename !! HDF5 output pathname.
   CHARACTER(256) :: planfile     !! Visualisation-plan pathname.
   CHARACTER(256) :: checkfile    !! Visualisation-plan diagnostic pathname.

   INTEGER, PARAMETER :: freelimit = 360000 !! Legacy DLL licensing threshold; equal to `szlimit` in current source.
   INTEGER, PARAMETER :: szlimit = 360000 !! Per-axis upper limit applied to `grid_nx` and `grid_ny`.
   CHARACTER(256)     :: dumtext          !! Private workspace for the grid-limit diagnostic.

   PRIVATE
   PUBLIC ::     north, east, south, west, &
             grid_nx, grid_ny, top_cell, nel, &
             SU_NUMBER, &
             BANK_NO, RIVER_NO, &
             IS_SQUARE, IS_BANK, IS_LINK, &
             EXISTS, SEND_P, DIRQQ, &
             nsed, ncon, ver, &
             ROOTDIR, hdf5filename, planfile, checkfile

CONTAINS

!> @brief Tests whether an element-number sentinel is positive.
!>
!> The function is elemental so scalar or array element numbers can be tested.
!> [[visualisation_metadata]] uses it to remove zero-valued inactive cells from
!> a plan mask.
!>
!> @warning
!> This is a positivity test only. It does not establish that `i<=nel`, that an
!> element has a particular class, or that any module array is allocated.
!> @endwarning
!>
!> @history
!> | Date | Author | Description |
!> |:-----|:-------|:------------|
!> | 2020-09-08 | SB | Imported the legacy existence-sentinel helper with the visualisation sources. |
!> @endhistory
   ELEMENTAL LOGICAL FUNCTION exists(i) RESULT(r)
      INTEGER, INTENT(IN) :: i !! Element number or sentinel to test.
      r = i > 0
   END FUNCTION exists

!> @brief Stores one keyed startup value in persistent visualisation state.
!>
!> `text` is an exact, case-sensitive dispatch key. `da` and `db` are mandatory
!> even for scalar transfers; current scalar calls pass zero. Exactly one
!> optional payload family must match the selected key.
!>
!> | Key family | Required payload | Destination and required prior state |
!> |:-----------|:-----------------|:-------------------------------------|
!> | `north`, `east`, `south`, `west` | `ii` | Corresponding native face-column scalar. |
!> | `grid_nx`, `grid_ny`, `top_cell`, `nel`, `nsed`, `ncon`, `ver` | `ii` | Corresponding size/count scalar. |
!> | `dirqq`, `rootdir`, `hdf5fname`, `planfile`, `checkfile` | `cc` | Corresponding 256-character scalar; longer values truncate. |
!> | `is_square`, `is_bank`, `is_link` | `L1(da)`, `da=nel` | New logical array; `nel` must already be set. |
!> | `su` | `d2(da,db)`, matching the stored grid | New display-number grid; both grid sizes must already be set. |
!> | `bank_no`, `river_no` | `d2(da,db)`, with `(da,db)=(nel,4)` | Newly allocated topology table; `nel` must already be set. |
!>
!> `grid_nx` and `grid_ny` stop when the supplied value exceeds `szlimit`.
!> Every other recognized key assigns or allocates directly. An unknown key
!> prints a diagnostic and executes `STOP`. The saved call counter is retained
!> from legacy debugging but has no observable current use.
!>
!> @warning
!> Optional arguments are dereferenced without `PRESENT` checks and array
!> extents are not validated against `da`, `db`, or the previously stored
!> dimensions. Array destinations must be unallocated. Violating any part of
!> the ordered contract can cause undefined optional-argument use, a shape or
!> allocation error, or invalid persistent state. The unknown-key diagnostic
!> also evaluates `TRIM(cc)` even when `cc` was not supplied.
!> @endwarning
!>
!> @warning
!> The legacy grid guard checks each axis separately against 360000 but accepts
!> zero/negative values and does not check `grid_nx*grid_ny` or available
!> memory. Its `I4` format cannot display the six-digit limit, so the fatal
!> message renders the limit as asterisks. Because `szlimit==freelimit`, the
!> separate “illegal copy” branch is unreachable in the current build.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2008-01-23 | Unknown | - | Added the dated DLL/license and grid-dimension checks. |
!> | 2020-09-08 | SB | - | Imported the keyed SHEGRAPH pass-through with the visualisation sources. |
!> | 2026-03-29 | SvB | - | Removed DEC conditionals and the `DLLEXPORT` directive for portable in-process use. |
!> | 2026-04-08 | SB | 4.6.1 | Retained the portable implementation in the IFX compiler update. |
!> | 2026-09-05 | SvB | - | Added IOSTAT checking for all allocated arrays. |
!> @endhistory
   SUBROUTINE send_p(text, ii, L1, d2, cc, da, db)
      integer, save :: coun = 0 !! Legacy saved call counter; its former debug print is disabled.
      INTEGER, INTENT(IN) :: da !! Declared first extent for an optional array payload; zero for scalar calls.
      INTEGER, INTENT(IN) :: db !! Declared second extent for `d2`; zero when no rank-two payload is sent.
      INTEGER, INTENT(IN), OPTIONAL :: ii !! Integer payload for integer-scalar keys.
      INTEGER, DIMENSION(da, db), INTENT(IN), OPTIONAL :: d2 !! Rank-two integer payload for grid/topology keys.
      LOGICAL, DIMENSION(da), INTENT(IN), OPTIONAL :: L1 !! Rank-one logical payload for element-class keys.
      CHARACTER(*), INTENT(IN) :: text !! Exact lowercase dispatch key.
      CHARACTER(*), INTENT(IN), OPTIONAL :: cc !! Character payload for directory/filename keys.

      INTEGER(KIND=I_P) :: ios
      CHARACTER(LEN=LENGTH_LINE) :: emsg !! ERRMSG= text from the failed (de)allocation.
      CHARACTER(LEN=*), PARAMETER :: location = 'send_p'

      coun = coun + 1
      SELECT CASE (text)
      CASE ('north'); north = ii
      CASE ('east'); east = ii
      CASE ('south'); south = ii
      CASE ('west'); west = ii
      CASE ('grid_nx')
         IF (szlimit > freelimit) PRINT *, 'THIS IS AN ILLEGAL COPY OF THE SHEGRAPH DLL 23/1/08'
         IF (ii > szlimit) THEN
            WRITE (dumtext, '(A,I4,A,I4,A)') '******* Grid size limit exceeded.  Limit is ', szlimit, ' by ', szlimit, ' cells'
            PRINT *, TRIM(dumtext)
            ERROR STOP
         ELSE
            grid_nx = ii
         END IF
      CASE ('grid_ny')
         IF (szlimit > freelimit) PRINT *, 'THIS IS AN ILLEGAL COPY OF THE SHEGRAPH DLL 23/1/08'
         IF (ii > szlimit) THEN
            WRITE (dumtext, '(A,I4,A,I4,A)') '******* Grid size limit exceeded.  Limit is ', szlimit, ' by ', szlimit, ' cells'
            PRINT *, TRIM(dumtext)
            ERROR STOP
         ELSE
            grid_ny = ii
         END IF

      CASE ('top_cell'); top_cell = ii
      CASE ('nel'); nel = ii
      CASE ('dirqq'); dirqq = cc

      CASE ('is_square')
         ALLOCATE (IS_SQUARE(nel), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "IS_SQUARE", location, emsg)
         IS_SQUARE = L1
      CASE ('is_bank')
         ALLOCATE (IS_BANK(nel), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "IS_BANK", location, emsg)
         IS_BANK = L1
      CASE ('is_link')
         ALLOCATE (IS_LINK(nel), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "IS_LINK", location, emsg)
         IS_LINK = L1
      CASE ('su')
         ALLOCATE (SU_NUMBER(grid_nx, grid_ny), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "SU_NUMBER", location, emsg)
         SU_NUMBER = d2
      CASE ('bank_no')
         ALLOCATE (BANK_NO(nel, 4), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "BANK_NO", location, emsg)
         BANK_NO = d2
      CASE ('river_no')
         ALLOCATE (RIVER_NO(nel, 4), STAT=ios, ERRMSG=emsg)
         CALL errstat_alloc(ios, "RIVER_NO", location, emsg)
         RIVER_NO = d2

      CASE ('nsed'); nsed = ii
      CASE ('ncon'); ncon = ii
      CASE ('ver'); ver = ii
      CASE ('rootdir'); rootdir = cc
      CASE ('hdf5fname'); hdf5filename = cc
      CASE ('planfile'); planfile = cc
      CASE ('checkfile'); checkfile = cc

      CASE DEFAULT; PRINT *, 'FAILED IN PASS  '//TRIM(text)//'  '//TRIM(cc); STOP

      END SELECT
   END SUBROUTINE send_p

END MODULE visualisation_pass
