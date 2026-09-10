!> summary: Readers for whole-grid integer and real arrays.
!> author: J. Ewen, Newcastle University; Stephen Birkinshaw, Newcastle University; Sven Berendsen
!>
!> [[AREADI]] and [[AREADR]] read a value per grid cell and map it onto the
!> element numbering, using the grid topology and the catchment mask to decide
!> which cells are active. They are the grid-shaped counterpart of the
!> record-oriented readers in [[record_readers]].
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2026-09-10 | SvB | - | Split out of utilsmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE grid_arrays

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, zero
   USE array_limits, ONLY: nelee, nxee, nyee
   USE element_geometry, ONLY: total_no_elements, total_no_links
   USE grid_topology, ONLY: NGDBGN, NX, NY, ICMXY, ICMREF
   USE channel_geometry, ONLY: ICMBK
   USE float_compare, ONLY: iszero_a, i_iszero_a2
   USE error_reporting, ONLY: ERR_STOP
   USE error_status, ONLY: errstat_read

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: AREADI, AREADR

CONTAINS

   !> Reads and optionally echoes an integer grid/element array.
   !>
   !> `AREADI` implements the legacy `KON` control modes for integer AL input:
   !> read a grid and convert it to element order, convert an existing element
   !> array back to a grid, or read and print a grid without conversion. The
   !> resulting integer element array is returned in `IAOUT`.
   !>
   !> Control modes are:
   !>
   !> | `KON` | Action |
   !> |:------|:-------|
   !> | 0 | Read grid array `IA`, convert it to element array `IAOUT`, and do not print. |
   !> | 1 | Read grid array `IA`, convert it to element array `IAOUT`, and print the grid array. |
   !> | 2 | Do not read; convert the input element array `IAOUT` back to grid array `IA` and print it. |
   !> | 3 | Fill `IAOUT(NGDBGN:total_no_elements)` with the default value supplied in `INF`; no file read is performed. |
   !>
   !> Parameters are:
   !>
   !> | Argument | Meaning |
   !> |:---------|:--------|
   !> | `IAOUT` | Integer element array returned by the routine, or input element array when `KON=2`. |
   !> | `KON` | Control parameter selecting read/convert/print/default-fill behaviour. |
   !> | `INF` | Input file unit for read modes; default integer value when `KON=3`. |
   !> | `IOF` | Output file unit used when printing the grid array. |
   !> | `INUM` | Expected range/count of integer codes; zero selects old `20I4` input. |
   !>
   !> For read modes, the grid-to-element mapping is
   !>
   !> \[
   !> IAOUT_{ICMXY(i,j)} = IA_{i,j}
   !> \quad\text{for each active grid cell } ICMXY(i,j)\ne0.
   !> \]
   !>
   !> For `KON=2`, the reverse reporting grid is assembled from grid elements using
   !>
   !> \[
   !> IA_{ICMREF(iel,2),ICMREF(iel,3)} = IAOUT_{iel}
   !> \quad\text{where } ICMREF(iel,1)=0.
   !> \]
   !>
   !> @note Only `KON=0`, `1`, and `3` are tested explicitly. Any other value uses
   !> the `KON=2` convert-and-print path. The single-digit integer grid format
   !> (`0 < INUM < 10`) is hard-limited to `NX <= 500`.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-09-28 | RAH | 3.4.1 | Added explicit `IMPLICIT` statement in the original source. |
   !> | 1995-07-24 | GP | 4.0 | Initialised `IAOUT` when `KON=0` or `KON=1`. |
   !> | 1997-08-04 | RAH | 4.1 | Added explicit typing; corrected `TITLE` from implicit double precision. |
   !> | 2026-04-03 | SvB | | Replaced numbered-`FORMAT`/labelled-`DO` I/O with named `DO` loops, inline `FORMAT` strings, and array-slice reads/assignments. |
   !> @endhistory
   SUBROUTINE AREADI(IAOUT, KON, INF, IOF, INUM)
!----------------------------------------------------------------------*
!
!      SERVICE SUBROUTINE TO READ AND PRINT AN INTEGER ARRAY
!
!----------------------------------------------------------------------*
      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: KON  !! Control parameter selecting read/convert/print/default-fill behaviour.
      INTEGER, INTENT(IN)  :: INF  !! Input file unit for read modes; default integer value when `KON=3`.
      INTEGER, INTENT(IN)  :: IOF  !! Output file unit used when printing the grid array.
      INTEGER, INTENT(IN)  :: INUM !! Expected range/count of integer codes; zero selects old `20I4` input.
      INTEGER, INTENT(OUT) :: IAOUT(:) !! Integer element array; also input when converting elements back to grid.
      INTEGER              :: I, I1, I2, IEL, J, K, L, LAL, LL1, NNX, NXX, ios
      INTEGER              :: IA(NXEE, NYEE)
      CHARACTER(4)         :: TITLE(20)
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'utilsmod:AREADI' !! Location string for read-error reports.
!----------------------------------------------------------------------*

!^^^^^^FILL IN SECTION
!
      IF (KON == 3) THEN
         ! Replaced DO loop with array slicing
         IAOUT(NGDBGN:total_no_elements) = INF
         RETURN
      END IF

!^^^^^^READ SECTION
!
! CHECK I/O FORMATS OK FOR PRINTING ARRAY (LIMIT CURRENTLY SET TO 200)
!
      IF ((INUM > 0 .AND. INUM < 10) .AND. NX > 500) THEN
         WRITE (IOF, "(' ', 'NX greater than 500. Change I/O formats in AREADI', /, 'Program aborted.')")
         CALL ERR_STOP(255)
      END IF

      IF (KON == 0 .OR. KON == 1) THEN
         READ (INF, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
         CALL errstat_read(ios, location, emsg)

         y_read_loop: DO I1 = 1, NY
            K = NY + 1 - I1
            IF (INUM > 0 .AND. INUM < 10) THEN
               ! Replaced implied DO loop with array slicing
               READ (INF, '(I7, 1X, 500I1)', IOSTAT=ios, IOMSG=emsg) I2, IA(1:NX, K)
               CALL errstat_read(ios, location, emsg)
               IF (I2 /= K) THEN
                  WRITE (IOF, "(/,/,2X, 'ERROR IN DATA ', 20A4, /,/,2X, 'IN THE VICINITY OF LINE K=', I5)") TITLE, I2
                  CALL ERR_STOP(255)
               END IF
            ELSE
               READ (INF, '(I7)', IOSTAT=ios, IOMSG=emsg) I2
               CALL errstat_read(ios, location, emsg)
               IF (I2 /= K) THEN
                  WRITE (IOF, "(/,/,2X, 'ERROR IN DATA ', 20A4, /,/,2X, 'IN THE VICINITY OF LINE K=', I5)") TITLE, I2
                  CALL ERR_STOP(255)
               END IF
               ! Note: Used list-directed read (*) as per your original commented-out line 30
               READ (INF, *, IOSTAT=ios, IOMSG=emsg) IA(1:NX, K)
               CALL errstat_read(ios, location, emsg)
            END IF
         END DO y_read_loop

!^^^^^^CONVERT GRID ARRAY TO ELEMENT ARRAY ...
!
         ! Replaced DO loop with array slicing
         IAOUT(1:total_no_elements) = 0

         grid_to_elem_x: DO I = 1, NX
            grid_to_elem_y: DO J = 1, NY
               IEL = ICMXY(I, J)
               IF (IEL /= 0) IAOUT(IEL) = IA(I, J)
            END DO grid_to_elem_y
         END DO grid_to_elem_x

!^^^^^^ ... OR CONVERT ELEMENT ARRAY TO GRID ARRAY
!
      ELSE
         ! Replaced nested DO 66 loops with modern array zeroing
         IA(1:NX, 1:NY) = 0

         elem_to_grid_loop: DO IEL = NGDBGN, total_no_elements
            IF (ICMREF(IEL, 1) == 0) THEN
               I = ICMREF(IEL, 2)
               J = ICMREF(IEL, 3)
               IA(I, J) = IAOUT(IEL)
            END IF
         END DO elem_to_grid_loop
      END IF

!^^^^^^PRINT SECTION
!
      IF (KON == 0) RETURN

      IF (KON == 1) WRITE (IOF, "(/, 20A4)") TITLE

! CHECK FOR ALL ZEROES
!
      IF (I_ISZERO_A2(IA(1:NX, 1:NY))) THEN
         WRITE (IOF, "(' ALL VALUES ZERO', /, ' ===============', /)")
         RETURN
      END IF

      NNX = (NX - 1)/10 + 1

      IF (INUM > 0 .AND. INUM < 10) THEN
         print_compact_loop: DO I1 = 1, NY
            K = NY + 1 - I1
            WRITE (IOF, "(' ', 'K=', I4, 1X, 500I1)") K, IA(1:NX, K)
         END DO print_compact_loop
      ELSE
         print_blocks_loop: DO L = 1, NNX
            LAL = L*10
            LL1 = LAL - 9
            ! Replaced MIN0 with modern generic MIN
            NXX = MIN(NX, LAL)

            WRITE (IOF, "('0', 9X, 10('J=',I3,6X), /)") (I, I=LL1, LAL)

            print_rows_loop: DO I1 = 1, NY
               K = NY + 1 - I1
               WRITE (IOF, "(' ', 'K=', I4, 2X, 10(I6,5X))") K, IA(LL1:NXX, K)
            END DO print_rows_loop
         END DO print_blocks_loop
      END IF

      WRITE (IOF, "(/,/,2X, 80('*'), /,/)")

   END SUBROUTINE AREADI

   !> Reads and optionally echoes a double-precision grid/element array.
   !>
   !> `AREADR` mirrors [[AREADI]] for floating-point input. Depending on `KON`, it
   !> reads grid values and converts them to SHETRAN element order, converts an
   !> existing element array for reporting, or reads and prints a grid directly.
   !>
   !> Control modes are:
   !>
   !> | `KON` | Action |
   !> |:------|:-------|
   !> | 0 | Read double-precision grid array `A`, convert it to element array `AOUT`, and do not print. |
   !> | 1 | Read double-precision grid array `A`, convert it to element array `AOUT`, and print the grid array. |
   !> | 2 | Do not read; convert the input element array `AOUT` back to grid array `A` and print it. |
   !>
   !> Parameters are:
   !>
   !> | Argument | Meaning |
   !> |:---------|:--------|
   !> | `AOUT` | Double-precision element array returned by the routine, or input element array when `KON=2`. |
   !> | `KON` | Control parameter selecting read/convert/print behaviour. |
   !> | `INF` | Input file unit for read modes. |
   !> | `IOF` | Output file unit used when printing the grid and link/bank values. |
   !>
   !> For read modes, the grid-to-element mapping is
   !>
   !> \[
   !> AOUT_{ICMXY(i,j)} = A_{i,j}
   !> \quad\text{for each active grid cell } ICMXY(i,j)\ne0.
   !> \]
   !>
   !> For `KON=2`, the reverse reporting grid is assembled from grid elements using
   !>
   !> \[
   !> A_{ICMREF(iel,2),ICMREF(iel,3)} = AOUT_{iel}
   !> \quad\text{where } ICMREF(iel,1)=0.
   !> \]
   !>
   !> Printed output also includes link values and their associated bank-element
   !> values through `ICMBK`.
   !>
   !> @note Only `KON=0` and `KON=1` read from `INF`; any other value uses the
   !> convert-and-print path. The all-zero print shortcut tests the element array
   !> `AOUT(1:total_no_elements)`, then printed output includes the grid plus
   !> link/bank values.
   !> @endnote
   !>
   !> @note `KON`, `INF`, and `IOF` are declared with no `INTENT` attribute here,
   !> unlike most other routines in this module. This is retained legacy F77-style
   !> behaviour, not something introduced by the recent modernisation.
   !> @endnote
   !>
   !> @history
   !> | Date | Author | Version | Description |
   !> |:-----|:-------|:--------|:------------|
   !> | 1994-09-28 | RAH | 3.4.1 | Added explicit `IMPLICIT` statement in the original source. |
   !> | 1997-08-04 | RAH | 4.1 | Added explicit typing; corrected `TITLE` from implicit double precision. |
   !> | 2026-04-03 | SvB | | Replaced numbered-`FORMAT`/labelled-`DO` I/O with named `DO` loops, inline `FORMAT` strings, and array-slice reads/assignments. |
   !> @endhistory
   SUBROUTINE AREADR(AOUT, KON, INF, IOF)
!----------------------------------------------------------------------*
!
!      SERVICE SUBROUTINE TO READ AND PRINT A DOUBLEPRECISION,TWO-DIMENSIONAL ARRAY
!      (IN DOUBLEPRECISION)
!
!----------------------------------------------------------------------*
! Commons and constants
      IMPLICIT NONE

! Input arguments
      INTEGER :: KON !! Control parameter selecting read/convert/print behaviour.
      INTEGER :: INF !! Input file unit for read modes.
      INTEGER :: IOF !! Output file unit used when printing the grid and link/bank values.

! In|out arguments
      DOUBLE PRECISION :: AOUT(NELEE) !! Double-precision element array; input when `KON` is not 0 or 1.

! Locals, etc
      INTEGER :: I, J, K, L, I1, I2, IEL, IEL1, IEL2, LAL, LL1, NNX, NXX, ios
      DOUBLE PRECISION :: B1, B2, A(NXEE, NYEE)
      CHARACTER(LEN=4) :: TITLE(20)
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      CHARACTER(LEN=*), PARAMETER :: location = 'utilsmod:AREADR' !! Location string for read-error reports.
!----------------------------------------------------------------------*

!^^^^^^READ SECTION
!
      IF (KON == 0 .OR. KON == 1) THEN
         READ (INF, '(20A4)', IOSTAT=ios, IOMSG=emsg) TITLE
         CALL errstat_read(ios, location, emsg)

         y_read_loop: DO I1 = 1, NY
            READ (INF, '(I7)', IOSTAT=ios, IOMSG=emsg) I2
            CALL errstat_read(ios, location, emsg)
            K = NY + 1 - I1

            IF (I2 /= K) THEN
               WRITE (IOF, "(/,/,2X, 'ERROR IN DATA ', 20A4, /,/,2X, 'IN THE VICINITY OF LINE K=', I5)") TITLE, I2
               CALL ERR_STOP(255)
            END IF

            ! 1. Replaced implied DO loop with array slicing
            READ (INF, '(10G7.0)', IOSTAT=ios, IOMSG=emsg) A(1:NX, K)
            CALL errstat_read(ios, location, emsg)
         END DO y_read_loop

!^^^^^^CONVERT GRID ARRAY TO ELEMENT ARRAY
!
         grid_to_elem_x: DO I = 1, NX
            grid_to_elem_y: DO J = 1, NY
               IEL = ICMXY(I, J)
               IF (IEL /= 0) AOUT(IEL) = A(I, J)
            END DO grid_to_elem_y
         END DO grid_to_elem_x

!^^^^^^CONVERT ELEMENT ARRAY TO GRID ARRAY
!
      ELSE
         ! 2. Replaced the nested DO 66 loops with modern array zeroing
         A(1:NX, 1:NY) = zero

         elem_to_grid_loop: DO IEL = NGDBGN, total_no_elements
            IF (ICMREF(IEL, 1) == 0) THEN
               I = ICMREF(IEL, 2)
               J = ICMREF(IEL, 3)
               A(I, J) = AOUT(IEL)
            END IF
         END DO elem_to_grid_loop
      END IF

!^^^^^^PRINT SECTION
!
      IF (KON == 0) RETURN

      IF (KON == 1) WRITE (IOF, "(/, 20A4)") TITLE

! CHECK FOR ALL ZEROES
!
      IF (ISZERO_A(AOUT(1:total_no_elements))) THEN
         WRITE (IOF, "(' ALL VALUES ZERO', /, ' ===============', /)")
         RETURN
      END IF

! PRINT ARRAY
!
      NNX = (NX - 1)/10 + 1

      print_blocks_loop: DO L = 1, NNX
         LAL = L*10
         LL1 = LAL - 9
         ! 3. Replaced MIN0 with modern generic MIN
         NXX = MIN(NX, LAL)

         WRITE (IOF, "('0', 9X, 10('J=',I3,6X), /)") (I, I=LL1, LAL)

         print_rows_loop: DO I1 = 1, NY
            K = NY + 1 - I1
            ! Replaced implied DO loop with array slicing
            WRITE (IOF, "(' ', 'K=', I4, 2X, 10G11.4)") K, A(LL1:NXX, K)
         END DO print_rows_loop
      END DO print_blocks_loop

      WRITE (IOF, "(/, 10X, 'LINK ', 6X, 'BANK1 ', 5X, 'BANK2 ', /)")

      link_print_loop: DO I = 1, total_no_links
         B1 = zero
         B2 = zero
         IEL1 = ICMBK(I, 1)
         IEL2 = ICMBK(I, 2)

         IF (IEL1 > 0) B1 = AOUT(IEL1)
         IF (IEL2 > 0) B2 = AOUT(IEL2)

         WRITE (IOF, "(1X, 'L= ', I4, 2X, 3G11.4)") I, AOUT(I), B1, B2
      END DO link_print_loop

      WRITE (IOF, "(/,/,2X, 120('*'), /,/)")

   END SUBROUTINE AREADR

END MODULE grid_arrays

