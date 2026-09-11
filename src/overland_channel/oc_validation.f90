!> summary: The `OCCHK0`--`OCCHK2` checks on the overland/channel input, and `OCLTL`.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> Three routines that check the overland/channel input for range and
!> consistency and report every failure through
!> [[error_reporting:RAISE_ERROR]], called from [[oc_driver:OCINI]] after the
!> data is read. [[OCLTL]] is the link-topology listing that frame setup
!> writes to the print file.
!>
!> These stay inside the component: the numbered diagnostics they issue are
!> overland/channel-specific.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of OCmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE oc_validation

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, ione1, izero1, zero, zero1
   USE array_limits, ONLY: nelee, nlfee, NOCTAB, nxee, NXSCEE
   USE element_geometry, ONLY: total_no_elements, total_no_links
   USE grid_topology, ONLY: ICMREF, ICMXY, NGDBGN, NX, NY
   USE file_units, ONLY: FID_logfile, OCD, OFB, OHB
   USE input_workspace, ONLY: IDUM
   USE oc_state, ONLY: LCODEX, LCODEY, STRXX, STRYY
   USE oc_boundaries, ONLY: NOCFB, NOCHB
   USE oc_cross_sections, ONLY: NXSECT, XINH, XINW
   USE oc_indexing, ONLY: LINKNO
   USE input_validation, ONLY: ALCHK, ALCHKI
   USE error_reporting, ONLY: RAISE_ERROR, ERR_STOP, ERRLVL_fatal, ERRLVL_error, ERRLVL_warn
   USE error_status, ONLY: errstat_read

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: OCCHK0, OCCHK1, OCCHK2, OCLTL

CONTAINS

!> @brief Checks OC file units, array bounds, and global entity counts.
!>
!> This is the first OC validation pass and ensures the output/input units
!> are usable and that compiled dimensions are large enough for the current
!> grid, channel-link, cross-section, and boundary-condition counts.
!>
!> Checks performed:
!>
!> | Check | Requirement |
!> |:------|:------------|
!> | `PRI`, `OCD` | Open formatted diagnostic/input units. |
!> | `NELEE` | At least `max(NX,total_no_elements)`. |
!> | `NLFEE` | At least `max(1,total_no_links)`. |
!> | `NXEE` | At least `NX`. |
!> | `NOCTAB` | At least 1. |
!> | `NXSCEE` | Greater than 1 for channel cross-section lookup tables. |
!> | `total_no_links` | Non-negative and less than `total_no_elements`. |
!> | `NX`, `NY` | Both at least 1. |
!> | `NGDBGN` | Equal to `total_no_links + 1`. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-01-30 | RAH | 4.2 | Created this routine. |
!> | 2009-01 | JE | - | Removed the `NELEE >= NOCTAB*NOCTAB` restriction. |
!> @endhistory
   SUBROUTINE OCCHK0()
      INTEGER       :: ERRNUM, I, IUNIT, NERR, OUNIT
      INTEGER, PARAMETER :: IUNDEF = 0
      INTEGER       :: IDUMS(1), IDUMO(1)
      LOGICAL       :: BOPEN, LDUM1(1)
      CHARACTER(47) :: MSG
      CHARACTER(11) :: FORM
      CHARACTER(3)  :: NAME
      NERR = 0
      !----------------------------------------------------------------------*
      ! 1. Unit Numbers
      ! ---------------
      ! PRI, OCD
      OUNIT = FID_logfile
      IUNIT = FID_logfile
      NAME = 'PRI'

      DO I = 0, 1
         INQUIRE (IUNIT, OPENED=BOPEN, FORM=FORM)

         IF (.NOT. BOPEN) THEN
            WRITE (MSG, '("File unit ",A," =",I4,1X,A)') NAME, IUNIT, 'is not connected to a file'
            ERRNUM = 1008
            IF (I == 0) OUNIT = 0
            CALL RAISE_ERROR(ERRLVL_error, ERRNUM, OUNIT, 0, 0, MSG)
            NERR = NERR + 1
         ELSE IF (FORM /= 'FORMATTED') THEN
            WRITE (MSG, '("File unit ",A," =",I4,1X,A,1X,A)') NAME, IUNIT, 'has format type', FORM
            ERRNUM = 1009
            IF (I == 0) OUNIT = 0
            CALL RAISE_ERROR(ERRLVL_error, ERRNUM, OUNIT, 0, 0, MSG)
            NERR = NERR + 1
         END IF

         ! Setup for the next iteration (I=1)
         IUNIT = OCD
         NAME = 'OCD'
      END DO

      IDUMS(1) = MIN(FID_logfile, OCD)

      CALL ALCHKI(ERRLVL_error, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ PRI, OCD ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)

      ! 2. Array Sizes
      ! --------------
      ! NELEE
      IDUMS(1) = NELEE
      IDUMO(1) = MAX(NX, total_no_elements)! , NOCTAB*NOCTAB)
      CALL ALCHKI(ERRLVL_error, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NELEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      ! NLFEE
      IDUMS(1) = NLFEE
      IDUMO(1) = MAX(1, total_no_links)
      CALL ALCHKI(ERRLVL_error, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLFEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      ! NXEE
      IDUMS(1) = NXEE
      IDUMO(1) = NX
      CALL ALCHKI(ERRLVL_error, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXEE', 'GE', IDUMO, IDUMS, NERR, LDUM1)
      ! NOCTAB
      IDUMS(1) = NOCTAB
      CALL ALCHKI(ERRLVL_error, 1001, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NOCTAB', 'GE', IONE1, IDUMS, NERR, LDUM1)
      ! NXSCEE
      IDUMS(1) = NXSCEE

      CALL ALCHKI(ERRLVL_error, 1002, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NXSCEE', 'GT', IONE1, IDUMS, NERR, LDUM1)

      ! 3. Number of Entities
      ! ---------------------
      ! NLF
      IDUMS(1) = total_no_links
      CALL ALCHKI(ERRLVL_error, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'GE', IZERO1, IDUMS, NERR, LDUM1)
      IDUMO(1) = total_no_elements
      CALL ALCHKI(ERRLVL_error, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NLF', 'LT', IDUMO, IDUMS, NERR, LDUM1)
      ! NX, NY
      IDUMS(1) = MIN(NX, NY)
      CALL ALCHKI(ERRLVL_error, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, '[ NX, NY ]', 'GE', IONE1, IDUMS, NERR, LDUM1)
      ! NGDBGN
      IDUMS(1) = NGDBGN
      IDUMO(1) = total_no_links + 1

      CALL ALCHKI(ERRLVL_error, 1003, OUNIT, 1, 1, IUNDEF, IUNDEF, 'NGDBGN', 'EQ', IDUMO, IDUMS, NERR, LDUM1)

      ! 4. Finish
      ! ---------
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 1000, OUNIT, 0, 0, 'Error(s) detected while checking OC input variables & constants')
      END IF

   END SUBROUTINE OCCHK0

!> @brief Checks static OC topology and channel-definition arrays.
!>
!> `OCCHK1` verifies neighbour references, active-grid indexing, and the
!> link-code grids used to locate north-south and east-west channel links.
!> Positive neighbour and grid references must not exceed
!> `total_no_elements`. Any `LCODEX` or `LCODEY` value in the
!> channel-boundary range 7:11 must map through `LINKNO` to a valid
!> channel-link element, i.e. an index greater than zero and less than
!> `NGDBGN`.
!>
!> Entry requirements:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `NEL >= 1`, `NX >= 1`, `NY >= 1` | The element and grid dimensions are populated. |
!> | `NELEE >= NEL`, `NXEE >= NX` | Workspace leading dimensions cover the active model extent. |
!> | `PRI` open for formatted output | Error reporting can write diagnostics. |
!> | `size_of_LDUM1 >= max(NX,NEL)` | Logical workspace is large enough for the largest check in this routine. |
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-03 | RAH | 4.2 | Created this routine. |
!> | 1998-02-05 | RAH | 4.2 | Added the `LDUM1` argument. |
!> @endhistory
   SUBROUTINE OCCHK1(SZLOG, LDUM1)

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN)  :: SZLOG        !! Size of the logical check-result workspace `LDUM1`.
      LOGICAL, INTENT(OUT) :: LDUM1(SZLOG) !! Discarded per-entry check-result scratch.

      ! Locals
      INTEGER :: CODE, FACE, I, IELx, X, Y, TYPEE
      INTEGER :: NERR, IUNDEF
      INTEGER :: IDUMO(1)

      CHARACTER(LEN=23) :: NAME
      CHARACTER, PARAMETER :: XY(0:1) = ['X', 'Y']

      !----------------------------------------------------------------------*

      ! Initialize local variables in the executable block to avoid implicit SAVE bugs
      NERR = 0
      IUNDEF = 0
      NAME = 'validity_of_LCODE?(x,y)'

      ! 1. Index Arrays
      ! ---------------
      IDUMO(1) = total_no_elements

      ! ICMREF
      face_loop: DO FACE = 1, 4
         CALL ALCHKI(ERRLVL_error, 1057, FID_logfile, 1, total_no_elements, FACE, 2, 'ICMREF(iel,face,2)', &
                     'LE', IDUMO, ICMREF(1:total_no_elements, 4 + FACE), NERR, LDUM1(1:total_no_elements))
      END DO face_loop

      ! ICMXY
      y_icmxy_loop: DO Y = 1, NY
         ! Modernized: Passing explicit array slice ICMXY(1:NX, Y) instead of scalar start point
         CALL ALCHKI(ERRLVL_error, 1057, FID_logfile, 1, NX, Y, IUNDEF, 'ICMXY(x,y)', &
                     'LE', IDUMO, ICMXY(1:NX, Y), NERR, LDUM1(1:NX))
      END DO y_icmxy_loop

      ! 2. Channel Definition Arrays
      ! ----------------------------
      ! LCODEX, LCODEY
      xy_loop: DO I = 0, 1

         ! Inject 'X' or 'Y' into the 18th character of the string
         NAME(18:18) = XY(I)

         y_lcode_loop: DO Y = 1, NY
            x_lcode_loop: DO X = 1, NX
               CODE = 0
               TYPEE = LCODEX(X, Y)*(1 - I) + LCODEY(X, Y)*I

               IF (TYPEE >= 7 .AND. TYPEE <= 11) THEN
                  IELx = LINKNO(X, Y, I == 0)
                  IF (IELx <= 0 .OR. IELx >= NGDBGN) CODE = TYPEE
               END IF

               IDUM(X) = CODE
            END DO x_lcode_loop

            ! Modernized: Explicit array slice for IDUM
            CALL ALCHKI(ERRLVL_error, 1058, FID_logfile, 1, NX, Y, IUNDEF, NAME, 'EQ', &
                        IZERO1, IDUM(1:NX), NERR, LDUM1(1:NX))
         END DO y_lcode_loop

      END DO xy_loop

      ! 3. Finish
      ! ---------
      IF (NERR > 0) THEN
         CALL RAISE_ERROR(ERRLVL_fatal, 1000, FID_logfile, 0, 0, 'Error(s) detected while checking static OC input arrays')
      END IF

   END SUBROUTINE OCCHK1

!> @brief Checks OC input values after [[OCREAD]].
!>
!> The checks cover boundary file units, overland/channel roughness values,
!> and channel cross-section tables, including monotonic level coordinates
!> and positive final widths.
!>
!> Boundary files `OHB` and `OFB` are checked only when their corresponding
!> boundary counts are non-zero. Roughness checks still call `ALCHK` with a
!> positive-roughness test for both `STRXX` and `STRYY`; however, the final
!> response to accumulated errors is a warning rather than a fatal error.
!> This preserves the current surface-storage convention where negative
!> `STRXX` values can be passed through to [[oc_stage_discharge:OCQDQ]].
!>
!> Channel cross-section checks require the first depth to be zero, depth
!> values to be strictly increasing, widths to be non-decreasing, and the
!> final width for each active link to be positive.
!>
!> Entry requirements retained from the legacy routine are:
!>
!> | Requirement | Meaning |
!> |:------------|:--------|
!> | `total_no_elements >= max(total_no_links,1)` | Active element range covers active links. |
!> | `NLFEE >= max(total_no_links,1)` | Link-indexed arrays cover active links. |
!> | `PRI` open for formatted output | Error reporting can write diagnostics. |
!>
!> @warning
!> `LDUM1` is declared `INTENT(INOUT)` here (the legacy routine declared it
!> `INTENT(IN)`) so it can be passed as the check-result buffer to
!> `ALCHK`/`ALCHKI`. The routine also carried a `USE` of the retired
!> `CONST_SY` module that its body never referenced; the import went with the
!> module.
!> @endwarning
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1998-02-03 | RAH | 4.2 | Created this routine, taking part of it from [[OCPLF]]. |
!> | 1998-02-06 | RAH | 4.2 | Added the boundary-file unit checks. |
!> | 1998-02-18 | RAH | 4.2 | Skipped the unit checks when `NONEED` is true. |
!> | 2022-05-19 | SB | - | Demoted the final error response from fatal to a warning, allowing the negative-`STRXX` surface-storage marker to pass through. |
!> @endhistory
   SUBROUTINE OCCHK2(DDUM1A, DDUM1B, SZLOG, LDUM1)


      IMPLICIT NONE

      INTEGER, INTENT(IN)           :: SZLOG          !! Size of the logical check-result workspace `LDUM1`.
      DOUBLE PRECISION, INTENT(OUT) :: DDUM1A(:)      !! Discarded cross-section lower-bound scratch.
      DOUBLE PRECISION, INTENT(OUT) :: DDUM1B(:)       !! Discarded cross-section upper-bound scratch.
      LOGICAL, INTENT(INOUT)        :: LDUM1(SZLOG)    !! Discarded per-entry check-result scratch; see the routine's warning.

      INTEGER :: ERRNUM, I, IELw, IUNDEF, IUNIT, N, NERR
      INTEGER :: IDUMS(1)
      LOGICAL :: BOPEN, NONEED
      CHARACTER(47) :: MSG
      CHARACTER(11)  :: FORM
      CHARACTER(3)   :: NAME
      CHARACTER(19)  :: SUBJ

      !----------------------------------------------------------------------*

      NERR = 0
      IUNDEF = 0

      ! 1. Unit Numbers
      ! ---------------
      ! OHB, OFB
      IDUMS(1) = 0
      IUNIT = OHB
      NAME = 'OHB'
      NONEED = NOCHB == 0

      DO I = 0, 1
         IF (.NOT. NONEED) THEN
            IDUMS(1) = MIN(IUNIT, IDUMS(1))
            INQUIRE (IUNIT, OPENED=BOPEN, FORM=FORM)

            IF (.NOT. BOPEN) THEN
               WRITE (MSG, 9100) NAME, IUNIT, 'is not connected to a file'
               ERRNUM = 1008
               CALL RAISE_ERROR(ERRLVL_error, ERRNUM, FID_logfile, 0, 0, MSG)
               NERR = NERR + 1
            ELSE IF (FORM /= 'FORMATTED') THEN
               WRITE (MSG, 9100) NAME, IUNIT, 'has format type', FORM
               ERRNUM = 1009
               CALL RAISE_ERROR(ERRLVL_error, ERRNUM, FID_logfile, 0, 0, MSG)
               NERR = NERR + 1
            END IF
         END IF

         ! Setup for OFB on the second pass
         IUNIT = OFB
         NAME = 'OFB'
         NONEED = NOCFB == 0
      END DO

      CALL ALCHKI(ERRLVL_error, 1003, FID_logfile, 1, 1, IUNDEF, IUNDEF, '[ OHB, OFB ]', 'GE', IZERO1, IDUMS, NERR, LDUM1)

      ! 2. Element Properties
      ! ---------------------
      ! STRX
      CALL ALCHK(ERRLVL_error, 1010, FID_logfile, 1, total_no_elements, IUNDEF, IUNDEF, 'STRX(iel)', 'GT', ZERO1, ZERO, STRXX, NERR, LDUM1)
      ! STRY
      CALL ALCHK(ERRLVL_error, 1010, FID_logfile, 1, total_no_elements, IUNDEF, IUNDEF, 'STRY(iel)', 'GT', ZERO1, ZERO, STRYY, NERR, LDUM1)

      ! 3. Cross-section Tables
      ! -----------------------
      !
      IF (total_no_links > 0) THEN
         ! XINH
         CALL ALCHK(ERRLVL_error, 1016, FID_logfile, 1, total_no_links, IUNDEF, IUNDEF, 'XINH(link)[j=1]', 'EQ', ZERO1, ZERO, XINH, NERR, LDUM1)

         DO IELw = 1, total_no_links
            N = NXSECT(IELw) - 1
            WRITE (SUBJ, 9310) IELw

            DDUM1A(1:N) = XINH(IELw, 1:N)
            DDUM1B(1:N) = XINH(IELw, 2:N + 1)
            CALL ALCHK(ERRLVL_error, 1017, FID_logfile, 1, N, IUNDEF, IUNDEF, SUBJ, 'GTa', DDUM1A, ZERO, DDUM1B, NERR, LDUM1)

            ! XINW
            SUBJ(4:4) = 'W'
            DDUM1A(1:N) = XINW(IELw, 1:N)
            DDUM1B(1:N) = XINW(IELw, 2:N + 1)
            CALL ALCHK(ERRLVL_error, 1017, FID_logfile, 1, N, IUNDEF, IUNDEF, SUBJ, 'GEa', DDUM1A, ZERO, DDUM1B, NERR, LDUM1)
         END DO

         DO IELw = 1, total_no_links
            DDUM1A(IELw) = XINW(IELw, NXSECT(IELw))
         END DO

      CALL ALCHK(ERRLVL_error, 1056, FID_logfile, 1, total_no_links, IUNDEF, IUNDEF, 'XINW[link,NXSECT(link)]', 'GT', ZERO1, ZERO, &
                    DDUM1A, NERR, LDUM1)
      END IF

      IF (NERR > 0) THEN
         ! sb 190522 negative strickler for surface storage
         CALL RAISE_ERROR(ERRLVL_warn, 1000, FID_logfile, 0, 0, 'Error(s) detected while checking OC input data')
         ! CALL ERROR(ERRLVL_fatal, 1000, FID_logfile, 0, 0, 'Error(s) detected while checking OC input data')
      END IF

      ! Format Statements
      ! -----------------
9100  FORMAT('File unit ', A, ' =', I4, 1X, A:1X, A)
9310  FORMAT('XINH[ link =', I3, '](j)')

   END SUBROUTINE OCCHK2

!> @brief Reads an alphanumeric channel-definition grid.
!>
!> `OCLTL` decodes the legacy one-character OC map into integer link and
!> boundary codes, preserving row-number checks and optional echo printing.
!> Input rows must be supplied from `NNY` down to 1; an unexpected row number
!> prints an "incorrect coordinate" marker when echo output is enabled and
!> then stops the program.
!>
!> Character mapping:
!>
!> | Character | Code | Meaning in OC flow-code grids |
!> |:----------|:-----|:-------------------------------|
!> | `I` | 1 | Internal impermeable boundary. |
!> | `.` | 2 | No special OC boundary/link code. |
!> | `R` | 6 | River/channel link without boundary type. |
!> | `W` | 7 | Channel weir boundary. |
!> | `A` | 8 | Channel river/resistance plus weir boundary. |
!> | `H` | 9 | Channel time-varying head boundary. |
!> | `F` | 10 | Channel time-varying flow boundary. |
!> | `P` | 11 | Channel polynomial boundary. |
!>
!> Characters not listed in `CODES` leave the target entry at its initial
!> zero value.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-08-12 | - | - | Created this routine. |
!> | 2015-04-21 | SB | - | Increased the `A1LINE` row buffer and its read/write format from 200 to 500 characters for larger catchments. |
!> @endhistory
   SUBROUTINE OCLTL(NNX, NNY, IARR, NXE, NYE, INF, IOF, BPCNTL)
      IMPLICIT NONE

      ! Dummy Arguments
      INTEGER, INTENT(IN)  :: NNX    !! X dimension of the grid to read.
      INTEGER, INTENT(IN)  :: NNY    !! Y dimension of the grid to read.
      INTEGER, INTENT(IN)  :: NXE    !! First declared extent of `IARR`.
      INTEGER, INTENT(IN)  :: NYE    !! Second declared extent of `IARR`.
      INTEGER, INTENT(IN)  :: INF    !! Input file unit for the OC map records.
      INTEGER, INTENT(IN)  :: IOF    !! Echo-output file unit.
      INTEGER, INTENT(OUT) :: IARR(NXE, NYE) !! Decoded OC flow-code grid; entries within `1:NNX,1:NNY` are overwritten.
      LOGICAL, INTENT(IN)  :: BPCNTL !! Enables echo printing and coordinate-error output.

      ! Local Variables
      CHARACTER(LEN=80)    :: TITLE
      CHARACTER(LEN=1)     :: A1LINE(500)
      INTEGER              :: I, J, K, L, M, ios
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'OCmod:OCLTL' !! Location string for read-error reports.

      CHARACTER(LEN=1), PARAMETER :: CODES(11) = &
                                     ['I', '.', ' ', ' ', ' ', 'R', 'W', 'A', 'H', 'F', 'P']

      READ (INF, '(A80)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      IF (BPCNTL) WRITE (IOF, '(A80)') TITLE

      IARR(1:NNX, 1:NNY) = 0

      I = NNY

      read_loop: DO J = 1, NNY
         READ (INF, '(I7, 1X, 500A1)', IOSTAT=ios, IOMSG=emsg) K, A1LINE(1:NNX)
         CALL errstat_read(ios, location, emsg)
         IF (BPCNTL) WRITE (IOF, '(I7, 1X, 500A1)') K, A1LINE(1:NNX)

         IF (K /= I) THEN
            IF (BPCNTL) WRITE (IOF, "('  ^^^   INCORRECT COORDINATE')")
            WRITE (*, '(A)') 'INCORRECT COORDINATE'
            CALL ERR_STOP(255)
         END IF

         I = I - 1

         line_loop: DO L = 1, NNX
            search_code: DO M = 1, 11
               IF (A1LINE(L) == CODES(M) .AND. CODES(M) /= ' ') THEN
                  IARR(L, K) = M
                  EXIT search_code
               END IF
            END DO search_code
         END DO line_loop

      END DO read_loop

   END SUBROUTINE OCLTL

END MODULE oc_validation

