!> summary: Reading and establishing the explicit bank elements.
!> author: GP, Newcastle University; AB / RAH, Newcastle University; JE, Newcastle University; SB, Newcastle University; Sven Berendsen
!>
!> [[INBK]] reads the optional bank-element data file and establishes the bank
!> elements' roughness, initial water level, vegetation category and snowpack
!> state. It is called once, from frame initialisation, and only when explicit
!> banks are enabled.
!>
!> This module holds exactly one procedure. `INBK` was private inside `FRmod`
!> and is public here because frame initialisation now has to call across a
!> module boundary to reach it.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1989--1998 | GP / AB / RAH | 2.0--4.2 | Developed the overland and channel flow component. |
!> | 2008-12 | JE | 4.3.5F90 | Converted the OC Fortran sources to Fortran 90. |
!> | 2020--2026 | SB / SvB | 4.5--4.6 | Added the ZQ reservoir tables, the abstracted state accessors, and the modernisation pass. |
!> | 2026-09-11 | SvB | - | Split out of FRmod; see docs/rename/proposal.md. |
!> @endhistory
MODULE bank_setup

   USE MOD_PARAMETERS, ONLY: LENGTH_LINE, zero
   USE array_limits, ONLY: nelee, nlfee, nxee, nyee
   USE element_geometry, ONLY: total_no_elements, total_no_links, ZGRUND
   USE grid_topology, ONLY: ICMREF, NGDBGN
   USE channel_geometry, ONLY: ZBFULL
   USE file_units, ONLY: BKD, FID_logfile
   USE met_forcing, ONLY: NMC, NRAINC
   USE run_control, ONLY: TITLE
   USE et_state, ONLY: NVC
   USE snow_state, ONLY: RHOSAR, SD
   USE vs_state, ONLY: ZVSPSL
   USE oc_state, ONLY: STRXX, STRYY
   USE oc_node_solver, ONLY: gethrf, sethrf
   USE error_reporting, ONLY: RAISE_ERROR, ERRLVL_fatal
   USE error_status, ONLY: errstat_read

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: INBK

CONTAINS

!> @brief Reads and initialises bank water-level/depth data.
!>
!> `INBK` reads bank-component input data and sets bank water-surface elevations
!> and related bank state used by OC, VSS, sediment, and contaminant routines.
!> The routine loops over 13 bank data records. The `INTYPE` input methods are:
!>
!> | `INTYPE` | Meaning |
!> |:---------|:--------|
!> | 1 | Copy from an adjacent grid element if possible, otherwise from the first adjacent bank element found on the second pass. Ground level is set from adjacent bank-full elevation. |
!> | 2 | Set all bank elements from one supplied default value. For ground level, the value is an offset from `ZBFULL`. |
!> | 3 | Unsupported; the routine raises fatal error 1061. |
!> | 4 | Read explicit `(bank element, value)` pairs. The read `NVALUE` is ignored and replaced by `2*total_no_links`. |
!>
!> | `IDATA` | Target | Type and transform |
!> |:--------|:-------|:-------------------|
!> | 1 | `ZGRUND` | Real. `INTYPE=1` sets `ZBFULL(link)`; `INTYPE=2` stores `ZBFULL(link)+DFAULT`; `INTYPE=4` stores the explicit elevation. |
!> | 2 | `NMC` | Integer meteorological category. |
!> | 3 | `NRAINC` | Integer rainfall category. |
!> | 4 | `NVC` | Integer vegetation category. |
!> | 5 | None | Integer value is read into workspace for `INTYPE=2/4` but is not applied. |
!> | 6 | `STRXX` | Real east-west Strickler/roughness value. |
!> | 7 | `STRYY` | Real north-south Strickler/roughness value. |
!> | 8 | None | Integer value is read into workspace for `INTYPE=2/4` but is not applied. |
!> | 9 | None | Integer value is read into workspace for `INTYPE=2/4` but is not applied. |
!> | 10 | `SD` | Initial bank-element snow depth (mm snow). |
!> | 11 | `RHOSAR` | Initial bank-element snow specific gravity (dimensionless). |
!> | 12 | `ZVSPSL` | Real. `INTYPE=1` copies adjacent phreatic elevation plus `ZGRUND(IEL)-ZGRUND(JEL)`; `INTYPE=2/4` interprets input as depth below bank ground and sets `ZGRUND-DUMMY`. |
!> | 13 | `HRF` | Real. `INTYPE=1` copies adjacent water-surface elevation plus `ZGRUND(IEL)-ZGRUND(JEL)`; `INTYPE=2/4` interprets input as water depth above bank ground and sets `ZGRUND+DUMMY`. |
!>
!> Bank widths are not set here. `INTEGR` selects integer input for records 2,
!> 3, 4, 5, 8, and 9; all other records are read as real values. The routine
!> uses bank input unit `BKD`, element references `ICMREF`, and bank-full
!> elevations `ZBFULL`, with `IDUM` and `DUMMY` as workspace.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-01 | RAH | 3.4.1 | Standardised inherited typing. |
!> | 1994-08 | GP | 4.0 | Moved VSS soil-layer state out of bank input. |
!> | 1998-07 | RAH | 4.2 | Removed unsupported class-based bank input. |
!> | 2009-01 | JE | - | Restructured loops for automatic differentiation. |
!> @endhistory
   SUBROUTINE INBK

      IMPLICIT NONE

      ! Locals, etc
      INTEGER :: I, IEL, ICOUNT, IDATA, IFAULT, IL, INTYPE, ITYPE
      INTEGER :: J, JEL, NVALUE, ios
      INTEGER :: IVALUE(NLFEE*2), IELEM(NLFEE*2)
      DOUBLE PRECISION :: DFAULT, DZG, VALUE(NLFEE*2)
      LOGICAL :: BINBKD, found_adjacent
      CHARACTER(LEN=LENGTH_LINE)  :: emsg !! `IOMSG=` text from a failed `READ`.
      INTEGER, DIMENSION(NXEE*NYEE), SAVE :: IDUM !! Integer input workspace; scratch within this routine only. `SAVE` keeps it in static storage, as the former module variable was.
      DOUBLEPRECISION, DIMENSION(NELEE), SAVE :: DUMMY !! Floating-point input workspace; scratch within this routine only. `SAVE` keeps it in static storage, as the former module variable was.
      CHARACTER(LEN=*), PARAMETER :: location = 'bank_setup:INBK' !! Location string for read-error reports.

      LOGICAL, PARAMETER :: INTEGR(13) = [.FALSE., .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., &
                                          .FALSE., .TRUE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE.]

      !
      ! READ TITLE, FLAG FOR PRINTING INITIALISATION DATA
      ! :BK1
      READ (BKD, '(A)', IOSTAT=ios, IOMSG=emsg) TITLE
      CALL errstat_read(ios, location, emsg)
      READ (BKD, '(L7)', IOSTAT=ios, IOMSG=emsg) BINBKD
      CALL errstat_read(ios, location, emsg)

      ! ----- LOOP OVER INPUT DATA TYPES
      !
      out500: DO IDATA = 1, 13
         ! INITIALISE DUMMY ARRAYS
         DO IEL = NGDBGN, total_no_elements
            IDUM(IEL) = 0
            DUMMY(IEL) = zero
         END DO

         ! READ TITLE, INPUT METHOD, NUMBER OF FOLLOWING VALUES
         ! :BK3
         READ (BKD, '(A)', IOSTAT=ios, IOMSG=emsg) TITLE
         CALL errstat_read(ios, location, emsg)
         IF (BINBKD) WRITE (FID_logfile, '(A)') TITLE
         READ (BKD, '(10I7)', IOSTAT=ios, IOMSG=emsg) INTYPE, NVALUE
         CALL errstat_read(ios, location, emsg)

         !
         ! TYPE 1: SET VALUE = VALUE AT ADJACENT GRID
         ! ++++++++++++++++++++++++++++++++++++++++++
         !
         ! (except ZGRUND     = ZBFULL(il)
         !     and ZVSPSL,HRF = value + ZGRUND - ZGRUND(jel) )
         !
         ! NB. CATCHMENT IS SCANNED TWICE. THE 2nd TIME THROUGH, ANY BANKS WITH
         !     NO ADJACENT GRID ARE GIVEN THE VALUE OF THE 1st ADJACENT BANK FOUND
         !
         IF (INTYPE == 1) THEN
            out95: DO ICOUNT = 1, 2
               out90: DO IEL = NGDBGN, total_no_elements
                  ITYPE = ICMREF(IEL, 1)
                  IF (ITYPE /= 1 .AND. ITYPE /= 2) CYCLE out90

                  ! * find adjacent element
                  found_adjacent = .FALSE.

                  out60: DO J = 1, 4
                     JEL = ICMREF(IEL, 4 + J)
                     IF (JEL > 0) THEN
                        IF (ICMREF(JEL, 1) == 0) THEN
                           found_adjacent = .TRUE.
                           EXIT out60
                        END IF
                     END IF
                  END DO out60

                  IF (.NOT. found_adjacent) THEN
                     out65: DO J = 1, 4
                        JEL = ICMREF(IEL, J + 4)
                        IF (JEL > 0) THEN
                           IF (ICMREF(JEL, 1) == 1 .OR. ICMREF(JEL, 1) == 2) THEN
                              found_adjacent = .TRUE.
                              EXIT out65
                           END IF
                        END IF
                     END DO out65
                  END IF

                  ! * set value
                  DZG = ZGRUND(IEL) - ZGRUND(JEL)

                  SELECT CASE (IDATA)
                  CASE (1)
                     IL = ICMREF(IEL, 4)
                     ZGRUND(IEL) = ZBFULL(IL)
                  CASE (2)
                     NMC(IEL) = NMC(JEL)
                  CASE (3)
                     NRAINC(IEL) = NRAINC(JEL)
                  CASE (4)
                     NVC(IEL) = NVC(JEL)
                  CASE (6)
                     STRXX(IEL) = STRXX(JEL)
                  CASE (7)
                     STRYY(IEL) = STRYY(JEL)
                  CASE (10)
                     SD(IEL) = SD(JEL)
                  CASE (11)
                     RHOSAR(IEL) = RHOSAR(JEL)
                  CASE (12)
                     ZVSPSL(IEL) = ZVSPSL(JEL) + DZG
                  CASE (13)
                     CALL SETHRF(IEL, GETHRF(JEL) + DZG)
                  END SELECT
               END DO out90
            END DO out95

            CYCLE out500
            !
            ! TYPE 2: READ SINGLE DEFAULT VALUE
            ! +++++++++++++++++++++++++++++++++
            !
         ELSE IF (INTYPE == 2) THEN
            ! :BK5
            IF (INTEGR(IDATA)) THEN
               READ (BKD, '(10I7)', IOSTAT=ios, IOMSG=emsg) IFAULT
               CALL errstat_read(ios, location, emsg)
               IF (BINBKD) WRITE (FID_logfile, 1300) IFAULT

               DO IEL = NGDBGN, total_no_elements
                  ITYPE = ICMREF(IEL, 1)
                  IF (ITYPE == 1 .OR. ITYPE == 2) IDUM(IEL) = IFAULT
               END DO
               ! :BK6
            ELSE
               READ (BKD, '(10F7.0)', IOSTAT=ios, IOMSG=emsg) DFAULT
               CALL errstat_read(ios, location, emsg)
               IF (BINBKD) WRITE (FID_logfile, 1500) DFAULT

               DO IEL = NGDBGN, total_no_elements
                  ITYPE = ICMREF(IEL, 1)
                  ! amended by GP 18/7/94 to be consistent with DSATE code
                  IF (ITYPE == 1 .OR. ITYPE == 2) THEN
                     IF (IDATA == 1) THEN
                        IL = ICMREF(IEL, 4)
                        DUMMY(IEL) = ZBFULL(IL) + DFAULT
                     ELSE
                        DUMMY(IEL) = DFAULT
                     END IF
                  END IF
               END DO
            END IF

            ! TYPE 3: READ PAIRS OF (DATA CLASS, VALUE)
            ! +++++++++++++++++++++++++++++++++++++++++
         ELSE IF (INTYPE == 3) THEN
            ! :BK7-8
            CALL RAISE_ERROR(ERRLVL_fatal, 1061, FID_logfile, 0, 0, 'BKD input type 3 (data class, value) not supported')

            ! TYPE 4: READ PAIRS OF (BANK ELEMENT NUMBER, VALUE)
            ! ++++++++++++++++++++++++++++++++++++++++++++++++++
         ELSE IF (INTYPE == 4) THEN
            NVALUE = 2*total_no_links
            ! 980713
            IF (INTEGR(IDATA)) THEN
               READ (BKD, '(10I7)', IOSTAT=ios, IOMSG=emsg) (IELEM(I), IVALUE(I), I=1, NVALUE)
               CALL errstat_read(ios, location, emsg)
               IF (BINBKD) WRITE (FID_logfile, 2000)
               IF (BINBKD) WRITE (FID_logfile, 2050) (IELEM(I), IVALUE(I), I=1, NVALUE)

               DO I = 1, NVALUE
                  IEL = IELEM(I)
                  ITYPE = ICMREF(IEL, 1)
                  IF (ITYPE == 1 .OR. ITYPE == 2) IDUM(IEL) = IVALUE(I)
               END DO
            ELSE
               READ (BKD, '(5(I7,F7.0))', IOSTAT=ios, IOMSG=emsg) (IELEM(I), VALUE(I), I=1, NVALUE)
               CALL errstat_read(ios, location, emsg)
               IF (BINBKD) WRITE (FID_logfile, 2100)
               IF (BINBKD) WRITE (FID_logfile, 2150) (IELEM(I), VALUE(I), I=1, NVALUE)

               DO I = 1, NVALUE
                  IEL = IELEM(I)
                  ITYPE = ICMREF(IEL, 1)
                  IF (ITYPE == 1 .OR. ITYPE == 2) DUMMY(IEL) = VALUE(I)
               END DO
            END IF
         END IF

         ! MOVE DATA FROM DUMMY ARRAYS INTO ACTUAL DATA ARRAYS
         DO IEL = NGDBGN, total_no_elements
            ITYPE = ICMREF(IEL, 1)
            IF (ITYPE == 1 .OR. ITYPE == 2) THEN
               SELECT CASE (IDATA)
               CASE (1)
                  ZGRUND(IEL) = DUMMY(IEL)
               CASE (2)
                  NMC(IEL) = IDUM(IEL)
               CASE (3)
                  NRAINC(IEL) = IDUM(IEL)
               CASE (4)
                  NVC(IEL) = IDUM(IEL)
               CASE (6)
                  STRXX(IEL) = DUMMY(IEL)
               CASE (7)
                  STRYY(IEL) = DUMMY(IEL)
               CASE (10)
                  SD(IEL) = DUMMY(IEL)
               CASE (11)
                  RHOSAR(IEL) = DUMMY(IEL)
               CASE (12)
                  ZVSPSL(IEL) = ZGRUND(IEL) - DUMMY(IEL)
               CASE (13)
                  CALL SETHRF(IEL, ZGRUND(IEL) + DUMMY(IEL))
               END SELECT
            END IF
         END DO

      END DO out500

      ! FORMAT STATEMENTS
      !
1300  FORMAT(' DEFAULT VALUE ', I7, ' USED IN ALL BANK ELEMENTS'/)
1500  FORMAT(' DEFAULT VALUE ', F12.3, ' USED IN ALL BANK ELEMENTS'/)
2000  FORMAT(' VALUES ALLOCATED TO EACH ELEMENT:'/3('       ELEMENT   VALUE'))
2050  FORMAT(3(I7, 2X, I7, 6X))
2100  FORMAT(' VALUES ALLOCATED TO EACH ELEMENT:'/3('       ELEMENT     VALUE'))
2150  FORMAT(3(I7, F12.3, 6X))

   END SUBROUTINE INBK

END MODULE bank_setup

