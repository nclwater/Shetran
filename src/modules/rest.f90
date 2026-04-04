MODULE rest
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
!                       Mops up .F files that do not have a natural home in any other module
   USE SGLOBAL
!USE SGLOBAL,    ONLY : NELEE, NVEE
   USE AL_G,    ONLY : icmref
   USE AL_C,    ONLY : ARXL, CWIDTH, CLAI,DELTAZ, DTUZ, EEVAP, ERUZ, tih, &
      NLYRBT, NV, &
      PLAI, PNETTO, QVSBF, QVSWEL,  QBKF, QOC, QVSH, UZNEXT, VSTHE, WBERR
   USE AL_D,    ONLY :  flerrc, balanc, syerrc, cmerrc, nstep, carea, DTMET2, BHOTRD, &
      BHOTTI, EPD, NM, PRD, NRAIN, DTMET3, PE, DTMET, MED, RN, OBSPE, &
      U, TA, VPD, TMAX, VHT, TIMEUZ, SD, PALFA, BEXSM, PMAX, precip_m_per_s, NRAINC, &
      tah, tal, ista
   USE ETmod,    ONLY : MODECS, CSTCAP, RELCST, TIMCST, NCTCST, CSTCA1, MODEPL, RELPLA, TIMPLA, NCTPLA, &
      PLAI1, MODECL, RELCLA, TIMCLA, NCTCLA, CLAI1, MODEVH, RELVHT, TIMVHT, NCTVHT, &
      VHT1, BMETP, BMETAL, BMETDATES, MEASPE, del
   USE FRmod,    ONLY : BSOFT
   USE mod_load_filedata,    ONLY : ALINIT
   USE UTILSMOD, ONLY : HOUR_FROM_DATE, TERPO1
   USE OCmod2,   ONLY : GETHRF
!USE PERTURBATIONS, ONLY : GETSPACETIME1
   IMPLICIT NONE

   LOGICAL :: FIRST_balwat=.TRUE.
   DOUBLEPRECISION :: STORW_balwat(NELEE)=zero, pinp(nvee+10)=zero, METIME=zero, MELAST=zero, EPTIME=zero


!INTEGER, PARAMETER :: mx_cnt_rain=1400
!INTEGER         :: cnt_rain = 0
!DOUBLEPRECISION :: start_impact_window, end_impact_window
!DOUBLEPRECISION :: per_rain(nvee, mx_cnt_rain)  !rain perturbations
!           Depths of water stored
!           For columns and stream links


   PRIVATE
   PUBLIC :: BALWAT, TMSTEP, EXTRA_OUTPUT, &
      metime, melast, eptime, pinp
!          start_impact_window, end_impact_window, per_rain, mx_cnt_rain, cnt_rain !these here for AD only
CONTAINS

!SSSSSS SUBROUTINE extra_output
   SUBROUTINE extra_output()
      INTEGER :: i
      DOUBLEPRECISION    :: car
      WRITE(PPPRI, 1400)
      DO I = 0, 100
         IF (FLERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 1000, FLERRC (I)
      END DO
      DO I = 0, 100
         IF (SYERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 2000, SYERRC (I)
      END DO
      DO I = 0, 100
         IF (CMERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 3000, CMERRC (I)
      END DO
      WRITE(PPPRI, 1600)
1400  FORMAT(// 'Error message asummary'/)
1500  FORMAT('No. of occurences of error number ',I4,': ',I6)

1600  FORMAT(/ 'End of error message asummary')
!<<<
      WRITE(PPPRI, '(////)')
      WRITE(PPPRI, 9900) UZNOW, NSTEP
!
      WRITE ( *, * )

      WRITE ( *, *) 'Normal completion of SHETRAN run'
!^^^^^sb 250105 mass balnce output
      WRITE(PPPRI, '(////)')
      WRITE(PPPRI,  * ) ' Spatially Averaged Totals (mm) over the simulation'
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Prec = ', balanc (7) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Can. Evap = ', balanc (8) * 1000 / &
         carea
      car = carea
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Soil+Sur Evp = ', balanc (9) &
         * 1000 / car
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Trans = ', balanc (10) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Cum Aqu. Flow = ', balanc (11) &
         * 1000 / carea

      WRITE(PPPRI, '(A20,F10.2)') 'Cum Discharge = ', balanc (12) &
         * 1000 / carea
      WRITE(PPPRI, '(//)')
      WRITE(PPPRI,  * ) ' Storage totals (mm) at the end of the simulation'
      WRITE(PPPRI, '(A20,F10.2)') 'Canopy Stor = ', balanc (13) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Snow Store = ', balanc (14) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Subsur Stor = ', balanc (15) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Surface Stor = ', balanc (16) * 1000 / &
         carea
      WRITE(PPPRI, '(A20,F10.2)') 'Channel Stor = ', balanc (17) * 1000 / &
         carea
9900  FORMAT ('Normal completion of SHETRAN run: ',F10.2, ' hours, ', &
      &        I7,' steps.' /)
   END SUBROUTINE extra_output




!SSSSSS SUBROUTINE BALWAT
   SUBROUTINE BALWAT
!----------------------------------------------------------------------*
!           Returns WBERR(column or link no.)
!           the balance error for water depth. This is the
!           extra depth, in metres, of water created during the
!           timestep.
!----------------------------------------------------------------------*
! Version:  SHETRAN/FR/BALWAT/4.1
! Modifications:
!  RAH  03.10.94  Version 3.4.1 from version 3.4 Aug 94: std header;
!                  declare everything; extra comments.
!                 Initialize STORW; set WBERR=0 on first pass.
!  GP  20.02.95  updated for SHETRAN V4.0 (finished 15/1/96)
!                   Mods for new VSS module: one loop for all elements;
!                   scrap AMULT,JBK,NLINKA,NLKSA; asumQ for advection;
!                   replace DDZ,TH3,RSZAQ,RSZWEL,QHSZ with DELTAZ,
!                   VSTHE,QVSBF,QVSWEL,QVSH (note change in sign); QBK*
!                   implicit in QVSH (except QBKF for link elements).
! RAH  970217  4.1  Swap subscripts: QVSH,DELTAZ,VSTHE (see AL.C).
!      970606       Rename locals NCE,NCL as CELL,IEL.
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P:            LLEE,NELEE,NLFEE
! Input common
!     AL.C:            LL,NLYRBT(NEL,1),AREA(NEL),CWIDTH(NLFEE)
!                      DELTAZ(LLEE,NEL),ZGRUND(NLF+1,NEL)
!                      DTUZ,ARXL(NLFEE),EEVAP(NEL),ERUZ(NELEE,LLEE)
!                      HRF(NLF+1:NEL),PNETTO(NEL),QBKF(NLFEE,2)
!                      QOC(NELEE,4),QVSBF(NEL),QVSH(4,LLEE,NEL)
!                      QVSWEL(NEL),VSTHE(LLEE,NEL),
!     AL.G:            NEL,ICMREF(NEL,1)
! In+out common
!     AL.C:            WBERR(NEL)
      DOUBLEPRECISION DELSTO, DEPTHI, DEPTHS, asum, asumQ
      INTEGER :: ITYPE, JDUM, CELL, IEL

!----------------------------------------------------------------------*
! Initialization
! --------------

      IF (FIRST_balwat) CALL ALINIT (ZERO, total_no_elements, WBERR)
! Loop Over Columns
! -----------------
      DO 400 IEL = 1, total_no_elements
         ITYPE = ICMREF (IEL, 1)
!        Calculate depth of water stored and change since previous step
!        --------------------------------------------------------------
!        * surface
         IF (ITYPE.EQ.3) THEN
            asum = ARXL (IEL) / CWIDTH (IEL)
         ELSE
            asum = GETHRF (IEL) - ZGRUND (IEL)

         ENDIF
!        * sub-surface
         DO 200 CELL = NLYRBT (IEL, 1), top_cell_no
            asum = asum + DELTAZ (CELL, IEL) * VSTHE (CELL, IEL)
200      END DO

         DEPTHS = asum
!        * net increase this timestep

         DELSTO = DEPTHS - STORW_balwat (IEL)
!        * save new value for use next timestep



         STORW_balwat (IEL) = DEPTHS
!        Calculate net depth of water supplied over the previous step
!        ------------------------------------------------------------
!        * ... but only if we have a bona fide value for DELSTO

         IF (FIRST_balwat) GOTO 400
!                     >>>>>>>>
!        * sources and sinks
         asum = PNETTO (IEL) - EEVAP (IEL) + QVSBF (IEL) - QVSWEL (IEL)
         DO 300 CELL = NLYRBT (IEL, 1), top_cell_no
            asum = asum - ERUZ (IEL, CELL)

300      END DO
!        * advection
         IF (ITYPE.EQ.3) THEN
            asumQ = - QBKF (IEL, 1) - QBKF (IEL, 2)
         ELSE
            asumQ = zero
         ENDIF
         DO 310 JDUM = 1, 2
            asumQ = asumQ - QOC (IEL, JDUM) + QOC (IEL, JDUM + 2)
            DO 305 CELL = NLYRBT (IEL, 1), top_cell_no
               asumQ = asumQ + QVSH (JDUM, CELL, IEL) + QVSH (JDUM + 2, &
                  CELL, IEL)
305         END DO
310      END DO

         asum = asum + asumQ / cellarea (IEL)
!        * convert from rate to depth


         DEPTHI = asum * DTUZ
!        Update the cumulative water balance error as a depth
!        ----------------------------------------------------

         WBERR (IEL) = WBERR (IEL) + DELSTO - DEPTHI



400   END DO
! Epilogue
! --------
      FIRST_balwat = .FALSE.
   END subroutine BALWAT




!SSSSSS SUBROUTINE METIN
   SUBROUTINE METIN (IFLAG)
!----------------------------------------------------------------------*
!
!  THIS SUBROUTINE READS IN THE MET DATA AS REQUIRED FOR THE
!  PENMAN-MONTEITH EQUATION, INTERCEPTION (AND SNOW MELT)
!  CALCULATIONS.  IT IS ASSUMED THAT A MET DATA PREPROGRAM
!  WILL HAVE CARRIED OUT VALIDATION CHECKS.
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/ET/METIN/4.1
! Modifications:
! RAH  941001 3.4.1 Add IMPLICIT DOUBLEPRECISION (see AL.P).
! RAH  961228  4.1  Initialize PELAST (was undefined). No long comments.
!      970516       Bring IDATA & PA from SPEC.ET; don't print values.
!                   Also bring EPLAST & PEIN.  Explicit typing.
!                   Generic intrinsics.  "PINP" not "P" in list below.
!                   Remove local TSTART (redundant), and
!                   TERPO1 redundant 7th arg (SPEC.ET arrays NUM*).
!----------------------------------------------------------------------*
! Commons and constants
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     VARIABLE AND UNIT SPEFICATION
!
!     ISITE - SITE IDENTIFIER                         NON-DIM
!     METIME- VALIDITY TIME OF CURRENT MET. DATA      HR
!     DTMET - TIMESTEP FOR INPUT OF MET. DATA         HR
!     PINP  - PRECIPITATION
!                    INPUT                            MM/HR
!                    IN THE CALCULATIONS              M/SEC
!     OBSPE - MEASURED POTENTIAL EVAPORATION
!                    INPUT                            MM/HR
!                    IN THE CALCULATIONS              MM/S
!     RN    - NET RADIATION                           W/M/M
!     U     - WIND SPEED                              M/S
!     TA    - AIR TEMPERATURE                         C
!     DEL   - SLOPE                                   MB/C
!     VPD   - VAPOUR PRESSURE DEFICIT                 MB
!970516  The following are read but not used:
!     PA    - ATM. PRESSURE                           MB.
!     IDATA - DATA QUALITY INDICATOR                  NON-DIM
!
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!     FOR BMETAL = .FALSE. :
!     IF THE NUMBER OF RAINFALL STATIONS AND THE NUMBER OF METEOROL-
!     OGICAL STATIONS ARE THE SAME, THE STATIONS ARE ASSUMED TO HAVE
!     THE SAME GRID-DISTRIBUTION CODE. METEOROLOGICAL AND RAINFALL
!     DATA ARE THEN GIVEN TOGETHER IN THE DATA FILE. IF THE NUMBERS
!     OF THE STATIONS ARE NOT THE SAME, RAINFALL AND METEOROLOGICAL
!     DATA ARE DISTRIBUTED BY SEPERATE GRID-CODES AND ARE READ FROM
!     SEPERATE LINES IN THE DATA FILE.
!
!     FOR BMETAL = .TRUE. :
!     EVAPOTR. DATA AND RAINFALL DATA ARE READ FROM TWO SEPARATE FILES.
!
!     NOTE: THE PRECIPITATION DATA IS AVERAGED OVER A COMPUTATIONAL
!           TIMESTEP ELSEWHERE
!
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
      IMPLICIT NONE

! Input arguments
      INTEGER, INTENT(IN) :: IFLAG

! Locals, etc
      INTEGER             :: I, IDATA, ISITE, K, NN
      DOUBLE PRECISION    :: EPLAST, TCURR, TEND
      DOUBLE PRECISION    :: PA(NVEE), PEIN(NVEE), PETOT(NVEE), PER(NRAIN)
      DOUBLE PRECISION    :: TAHIGHT(NVEE), TALOWT(NVEE), TAHIGH(NVEE), TALOW(NVEE)
      LOGICAL             :: FIRSTNOPRD = .TRUE., FIRSTNOEPD1 = .TRUE., FIRSTNOEPD2 = .TRUE.
      LOGICAL             :: FIRSTNOMET1 = .TRUE., FIRSTNOMET2 = .TRUE., FIRSTNOMET3 = .TRUE.
      LOGICAL             :: FIRSTNOMET4 = .TRUE., FIRSTNOMET5 = .TRUE.
      INTEGER             :: prdyear, prdmonth, prdday, prdhour, prdminute, prdsecond
      INTEGER             :: epdyear, epdmonth, epdday, epdhour, epdminute, epdsecond
      INTEGER             :: tahyear, tahmonth, tahday, tahhour, tahminute, tahsecond
      INTEGER             :: talyear, talmonth, talday, talhour, talminute, talsecond
      CHARACTER(LEN=100000) :: tmp
      INTEGER             :: ios
      DOUBLE PRECISION    :: prddate, epddate, tahdate, taldate
!----------------------------------------------------------------------*

      IF (.NOT. BMETAL) GOTO 40

! READ PREC. & OBSERVED POT. EVAPOTRANSPIRATION BREAKPOINT FORMAT FILES
!-----------------------------------------------------------------------

! PRECIPITATION
! read only one line of file (unless hotstarted run)

      IF (IFLAG == 1) THEN
         precip_read_loop: DO
            IF (BMETDATES) THEN
               READ(prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', IOSTAT=ios) &
                  prdyear, prdmonth, prdday, prdhour, prdminute, prdsecond, tmp

               IF (ios > 0) THEN
                  WRITE (*, '(A,I0,A)') ' Error reading the precipitation time series file. ' // &
                     'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ', &
                     NRAIN, ' values on each row'
                  WRITE(*, "('paused, type [enter] to continue')")
                  READ (*, *)
                  STOP
               END IF

               IF (ios < 0) THEN
                  IF (FIRSTNOPRD) THEN
                     WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                        'Time = ', uznow, ' Hours.', 'Finish of prd data', 'All remaining values will be zero'
                     FIRSTNOPRD = .FALSE.
                  END IF
                  PINP(1:NRAIN) = ZERO
               ELSE
                  prddate = HOUR_FROM_DATE(prdyear, prdmonth, prdday, prdhour, prdminute)
                  READ (tmp, *, IOSTAT=ios) PINP(1:NRAIN)

                  IF (ios > 0) THEN
                     WRITE (*, '(A,I0,A)') ' Error reading the precipitation time series file. ' // &
                        'This should have the date in the iso 8601 format followed by ', NRAIN, ' values'
                     WRITE(*, "('paused, type [enter] to continue')")
                     READ (*, *)
                     STOP
                  END IF
               END IF

            ELSE
               READ (PRD, *, IOSTAT=ios) PINP(1:NRAIN)

               IF (ios > 0) THEN
                  WRITE (*, '(A,I0,A)') ' Error reading the precipitation time series file. This should have ', &
                     NRAIN, ' values on each row with no dates in the first column (see ET1)'
                  WRITE(*, "('paused, type [enter] to continue')")
                  READ (*, *)
                  STOP
               END IF

               IF (ios < 0) THEN
                  IF (FIRSTNOPRD) THEN
                     WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                        'Time = ', uznow, ' Hours.', 'Finish of prd data', 'All remaining values will be zero'
                     FIRSTNOPRD = .FALSE.
                  END IF
                  PINP(1:NRAIN) = ZERO
               END IF
            END IF

            PINP(1:NRAIN) = PINP(1:NRAIN) / dtmet2
            MELAST = METIME
            METIME = METIME + dtmet2

            IF (.NOT. (BHOTRD .AND. METIME < BHOTTI)) EXIT precip_read_loop
         END DO precip_read_loop

      ELSE
         ! POT. EVAP and TEMPERATURE DATA READ PART 1

         ! first check for hotstarted run
         IF (BHOTRD .AND. EPTIME < BHOTTI) THEN
            hotstart_epd_loop: DO
               ! epd and temperature files have dates
               IF (BMETDATES) THEN
                  READ(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', IOSTAT=ios) &
                     epdyear, epdmonth, epdday, epdhour, epdminute, epdsecond, tmp

                  IF (ios > 0) THEN
                     WRITE (*, '(A,I0,A)') ' Error reading the potential evaporation time series file. ' // &
                        'This should have the date in iso 8601 format followed by ', NM, ' values on each row'
                     WRITE(*, "('paused, type [enter] to continue')")
                     READ (*, *)
                     STOP
                  END IF

                  IF (ios < 0) THEN
                     IF (FIRSTNOEPD2) THEN
                        WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                           'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                        FIRSTNOEPD2 = .FALSE.
                     END IF
                     PEIN(1:NM) = ZERO
                  ELSE
                     epddate = HOUR_FROM_DATE(epdyear, epdmonth, epdday, epdhour, epdminute)
                     READ (tmp, *, IOSTAT=ios) PEIN(1:NM)
                     IF (ios > 0) THEN
                        WRITE (*, '(A,I0,A)') ' Error reading potential evap data values from line.'
                        WRITE(*, "('paused, type [enter] to continue')")
                        READ (*, *)
                        STOP
                     END IF
                  END IF

                  IF (ISTA) THEN
                     READ(TAH, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', IOSTAT=ios) &
                        tahyear, tahmonth, tahday, tahhour, tahminute, tahsecond, tmp
                     IF (ios > 0) THEN
                        WRITE (*, '(A,I0,A)') ' Error reading max temp time series file.'
                        STOP
                     END IF
                     IF (ios < 0) TAHIGH(1:NM) = 10.0d0

                     IF (ios == 0) THEN
                        READ (tmp, *, IOSTAT=ios) TAHIGH(1:NM)
                        IF (ios > 0) STOP 'Error parsing max temperature line'
                        tahdate = HOUR_FROM_DATE(tahyear, tahmonth, tahday, tahhour, tahminute)
                     END IF
                  END IF

                  IF (ISTA) THEN
                     READ(TAL, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', IOSTAT=ios) &
                        talyear, talmonth, talday, talhour, talminute, talsecond, tmp
                     IF (ios > 0) THEN
                        WRITE (*, '(A,I0,A)') ' Error reading min temp time series file.'
                        STOP
                     END IF
                     IF (ios < 0) TALOW(1:NM) = 10.0d0

                     IF (ios == 0) THEN
                        READ (tmp, *, IOSTAT=ios) TALOW(1:NM)
                        IF (ios > 0) STOP 'Error parsing min temperature line'
                        taldate = HOUR_FROM_DATE(talyear, talmonth, talday, talhour, talminute)
                     END IF
                  END IF

                  PEIN(1:NM) = PEIN(1:NM) / dtmet3
                  EPLAST = EPTIME
                  EPTIME = EPTIME + dtmet3

                  IF (.NOT. (BHOTRD .AND. EPTIME < BHOTTI)) EXIT hotstart_epd_loop

                  ! epd and temperature files DO NOT have dates
               ELSE
                  READ (EPD, *, IOSTAT=ios) PEIN(1:NM)
                  IF (ios > 0) THEN
                     WRITE (*, '(A,I0,A)') ' Error reading the potential evaporation time series file. This should have ', &
                        NM, ' values on each row with no dates in the first column'
                     STOP
                  END IF

                  IF (ios < 0) THEN
                     IF (FIRSTNOEPD1) THEN
                        WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                           'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                        FIRSTNOEPD1 = .FALSE.
                     END IF
                     PEIN(1:NM) = ZERO
                  END IF

                  IF (ISTA) THEN
                     READ (TAH, *, IOSTAT=ios) TAHIGH(1:NM)
                     IF (ios > 0) STOP 'Error reading max temp file'
                     IF (ios < 0) TAHIGH(1:NM) = 10.0d0
                  END IF

                  IF (ISTA) THEN
                     READ (TAL, *, IOSTAT=ios) TALOW(1:NM)
                     IF (ios > 0) STOP 'Error reading min temp file'
                     IF (ios < 0) TALOW(1:NM) = 10.0d0
                  END IF

                  PEIN(1:NM) = PEIN(1:NM) / dtmet3
                  EPLAST = EPTIME
                  EPTIME = EPTIME + dtmet3

                  IF (.NOT. (BHOTRD .AND. EPTIME < BHOTTI)) EXIT hotstart_epd_loop
               END IF
            END DO hotstart_epd_loop
         END IF
         ! end of check for hotstarted run

         ! calculate average PE value over computational timestep
         TEND = MIN(UZNOW + UZNEXT, EPTIME)
         PETOT(1:NM) = (TEND - UZNOW) * PEIN(1:NM)

         ! POT. EVAP and TEMPERATURE DATA READ PART 2
         ! check if it is time to read in PET data
         IF (EPTIME < UZNOW + UZNEXT) THEN
            pet_read_loop: DO
               ! epd and temperature files have dates
               IF (BMETDATES) THEN
                  READ(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', IOSTAT=ios) &
                     epdyear, epdmonth, epdday, epdhour, epdminute, epdsecond, tmp

                  IF (ios > 0) STOP 'Error reading PET file'

                  IF (ios < 0) THEN
                     IF (FIRSTNOEPD2) THEN
                        WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                           'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                        FIRSTNOEPD2 = .FALSE.
                     END IF
                     PEIN(1:NM) = ZERO
                  ELSE
                     epddate = HOUR_FROM_DATE(epdyear, epdmonth, epdday, epdhour, epdminute)
                     READ (tmp, *, IOSTAT=ios) PEIN(1:NM)
                     IF (ios > 0) STOP 'Error parsing PET values'
                  END IF

                  IF (ISTA) THEN
                     READ(TAH, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', IOSTAT=ios) &
                        tahyear, tahmonth, tahday, tahhour, tahminute, tahsecond, tmp
                     IF (ios > 0) STOP 'Error reading max temp file'
                     IF (ios < 0) TAHIGH(1:NM) = 10.0d0

                     IF (ios == 0) THEN
                        READ (tmp, *, IOSTAT=ios) TAHIGH(1:NM)
                        IF (ios > 0) STOP 'Error parsing max temp values'
                        tahdate = HOUR_FROM_DATE(tahyear, tahmonth, tahday, tahhour, tahminute)
                     END IF
                  END IF

                  IF (ISTA) THEN
                     READ(TAL, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', IOSTAT=ios) &
                        talyear, talmonth, talday, talhour, talminute, talsecond, tmp
                     IF (ios > 0) STOP 'Error reading min temp file'
                     IF (ios < 0) TALOW(1:NM) = 10.0d0

                     IF (ios == 0) THEN
                        READ (tmp, *, IOSTAT=ios) TALOW(1:NM)
                        IF (ios > 0) STOP 'Error parsing min temp values'
                        taldate = HOUR_FROM_DATE(talyear, talmonth, talday, talhour, talminute)
                     END IF
                  END IF

                  PEIN(1:NM) = PEIN(1:NM) / dtmet3
                  EPLAST = EPTIME
                  EPTIME = EPTIME + dtmet3
                  TEND = MIN(UZNOW + UZNEXT, EPTIME)
                  PETOT(1:NM) = PETOT(1:NM) + (TEND - EPLAST) * PEIN(1:NM)

                  IF (.NOT. (EPTIME < UZNOW + UZNEXT)) EXIT pet_read_loop

                  ! epd and temperature files DO NOT have dates
               ELSE
                  READ (EPD, *, IOSTAT=ios) PEIN(1:NM)
                  IF (ios > 0) STOP 'Error reading PET file'

                  IF (ios < 0) THEN
                     IF (FIRSTNOEPD2) THEN
                        WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                           'Time = ', uznow, ' Hours.', 'Finish of epd data', 'All remaining values will be zero'
                        FIRSTNOEPD2 = .FALSE.
                     END IF
                     PEIN(1:NM) = ZERO
                  END IF

                  IF (ISTA) THEN
                     READ (TAH, *, IOSTAT=ios) TAHIGH(1:NM)
                     IF (ios > 0) STOP 'Error reading max temp file'
                     IF (ios < 0) TAHIGH(1:NM) = 10.0d0
                  END IF

                  IF (ISTA) THEN
                     READ (TAL, *, IOSTAT=ios) TALOW(1:NM)
                     IF (ios > 0) STOP 'Error reading min temp file'
                     IF (ios < 0) TALOW(1:NM) = 10.0d0
                  END IF

                  PEIN(1:NM) = PEIN(1:NM) / dtmet3
                  EPLAST = EPTIME
                  EPTIME = EPTIME + dtmet3
                  TEND = MIN(UZNOW + UZNEXT, EPTIME)
                  PETOT(1:NM) = PETOT(1:NM) + (TEND - EPLAST) * PEIN(1:NM)

                  IF (.NOT. (EPTIME < UZNOW + UZNEXT)) EXIT pet_read_loop
               END IF
            END DO pet_read_loop
         END IF

         OBSPE(1:NM) = PETOT(1:NM) / UZNEXT / 3600.0d0
         ! for simplicity the temperature used is the value at the end of the timestep
         TA(1:NM) = (TAHIGH(1:NM) + TALOW(1:NM)) / 2.0d0

      END IF

! PRINT OUT INPUT DATA
      IF (BMETP) THEN
         WRITE(PPPRI, "(/,/, 1X, 'MET DATA -  TIME :', F8.2, /, ' STATION           RAINFALL      POT. EVAP.(MM/HR)')") METIME
         DO I = 1, NM
            WRITE(PPPRI, "(4X,I2,9X,F10.3,9X,F10.3)") I, PINP(I), PEIN(I)
         END DO
      END IF

      GOTO 190

! READ ALL MET. DATA IN FIXED TIME INTERVAL (USUALLY HOURLY) FORMAT
!------------------------------------------------------------------
40    IF (IFLAG == 2) RETURN

      IF (NRAIN /= NM) GOTO 100

!-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE EQUAL
      IF (BMETP) WRITE(PPPRI, "(/,/,1X, 'MET DATA - SITE    TIME      RAINFALL    NET RADN', 4X, " // &
         "'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')")

!-----LOOP ON NUMBER OF MET SITES
55    MELAST = METIME
      METIME = METIME + DTMET

      DO I = 1, NM
         READ (MED, "(2I6, 4G12.6, /, 12X, 3G12.6, I12)", IOSTAT=ios) &
            ISITE, NN, PINP(I), RN(I), U(I), PA(I), TA(I), DEL(I), VPD(I), IDATA

         IF (ios < 0) THEN
            IF (FIRSTNOMET1) THEN
               WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                  'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
               FIRSTNOMET1 = .FALSE.
            END IF
            ISITE = 1
            NN = 1
            PINP(I) = ZERO
            RN(I) = ZERO
            U(I) = ZERO
            PA(I) = ZERO
            TA(I) = 10.0d0
            DEL(I) = ONE
            VPD(I) = 3.0d0
            IDATA = 1000
         END IF

         IF (BMETP) WRITE(PPPRI, "('0', 8X, I6, F8.2, 5X, 2(3F12.6,'  NOT_USED  '))") &
            ISITE, METIME, PINP(I), RN(I), U(I), TA(I), DEL(I), VPD(I)

         IF (MEASPE(I) == 0) CYCLE

! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
         READ (MED, "(12X, G12.6)", IOSTAT=ios) OBSPE(I)
         IF (ios < 0) THEN
            IF (FIRSTNOMET2) THEN
               WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                  'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
               FIRSTNOMET2 = .FALSE.
            END IF
            OBSPE(I) = 0.0d0
         END IF

! CONVERT TO MM/S
         OBSPE(I) = OBSPE(I) / 3600.0d0
      END DO

! READ TO START SIMULATION TIME, IF HOTSTART
      IF (BHOTRD .AND. METIME < BHOTTI) GOTO 55
      GOTO 190

!-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE UNEQUAL
100   IF (BMETP) WRITE(PPPRI, "(/,/, 1X, 'MET DATA - SITE    TIME      NET RADN', 4X, " // &
         "'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')")

!-----LOOP ON NUMBER OF MET SITES
115   MELAST = METIME
      METIME = METIME + DTMET

      DO I = 1, NM
         READ (MED, "(2I6, 12X, 3G12.6, /, 12X, 3G12.6, I12)", IOSTAT=ios) &
            ISITE, NN, RN(I), U(I), PA(I), TA(I), DEL(I), VPD(I), IDATA

         IF (ios < 0) THEN
            IF (FIRSTNOMET3) THEN
               WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                  'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
               FIRSTNOMET3 = .FALSE.
            END IF
            ISITE = 1
            NN = 1
            RN(I) = ZERO
            U(I) = ZERO
            PA(I) = ZERO
            TA(I) = 10.0d0
            DEL(I) = ONE
            VPD(I) = 3.0d0
            IDATA = 1000
         END IF

         IF (BMETP) WRITE(PPPRI, "('0', 8X, I6, F8.2, 5X, 2(2F12.6,'  NOT_USED  ':F12.6))") &
            ISITE, METIME, RN(I), U(I), TA(I), DEL(I), VPD(I)

         IF (MEASPE(I) == 0) CYCLE

! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
         READ (MED, "(12X, G12.6)", IOSTAT=ios) OBSPE(I)
         IF (ios < 0) THEN
            IF (FIRSTNOMET4) THEN
               WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                  'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
               FIRSTNOMET4 = .FALSE.
            END IF
            OBSPE(I) = 0.0d0
         END IF

! CONVERT TO MM/S
         OBSPE(I) = OBSPE(I) / 3600.0d0
      END DO

      IF (BMETP) WRITE(PPPRI, "(/,/, 1X, 'RAIN DATA - SITE    TIME      RAINFALL         IDATA')")

!-----LOOP ON NUMBER OF RAIN SITES
      DO I = 1, NRAIN
         READ (MED, "(2I6, G12.6, 24X, I12)", IOSTAT=ios) ISITE, NN, PINP(I), IDATA
         IF (ios < 0) THEN
            IF (FIRSTNOMET5) THEN
               WRITE(PPPRI, "(/,/,/, A6,g12.4,a8, /, A18, /, A33, /,/,/)") &
                  'Time = ', uznow, ' Hours.', 'Finish of met data', 'All remaining values will be zero'
               FIRSTNOMET5 = .FALSE.
            END IF
            PINP(I) = 0.0d0
         END IF

         IF (BMETP) WRITE(PPPRI, "('0', 9X, I6, F8.2, 5X, F12.6, '  NOT_USED  ')") ISITE, METIME, PINP(I)
      END DO

! READ TO SIMULATION START TIME, IF HOTSTART
      IF (BHOTRD .AND. METIME < BHOTTI) GOTO 115

!--------------------------------------------
!     CHECK TIME-VARYING MODEL PARAMETERS
!--------------------------------------------
190   TCURR = TIMEUZ
      DO K = 1, NV
         ! sb 04032025 for dynamically allocated arrays use NV not NVEE
         IF (MODECS(K) /= 0) CALL TERPO1(CSTCAP, TCURR, RELCST, TIMCST, NCTCST, CSTCA1, NV, K)
         IF (MODEPL(K) /= 0) CALL TERPO1(PLAI, TCURR, RELPLA, TIMPLA, NCTPLA, PLAI1, NV, K)
         IF (MODECL(K) /= 0) CALL TERPO1(CLAI, TCURR, RELCLA, TIMCLA, NCTCLA, CLAI1, NV, K)
         IF (MODEVH(K) /= 0) CALL TERPO1(VHT, TCURR, RELVHT, TIMVHT, NCTVHT, VHT1, NV, K)
      END DO

      RETURN

   END SUBROUTINE METIN



!SSSSSS SUBROUTINE TMSTEP
   SUBROUTINE TMSTEP
!----------------------------------------------------------------------*
!
!  COMPUTE THE NEXT TiMeSTEP AND READ METEOROLOGICAL DATA.
!
!----------------------------------------------------------------------*
! Version:  SHETRAN/FR/TMSTEP/4.2
! Modifications since v3.3:
!  GP Jul 93  3.4  Rewrite UZNEXT algorithm: scrap inputs PINMAX,PMAX,
!                  PREST; new inputs NSTEP,BSOFT; new locals EXIT,
!                  TSOFT,TSTART,TEND; many diffs.
!                  Call ERROR if UZNEXT too small.
!                  Call METIN twice, and pass arg IFLAG (see METIN).
! RAH 941003 3.4.1 Bring IMPLICIT DOUBLEPRECISION from SPEC.AL(AL.P).
!  GP 960717  4.0  Constrain UZNEXT.le.TSNOW (new local, also SMFLAG,
!                  IEL); uses new inputs BEXSM,NM,TA,NLF,NEL,SD.
! RAH 981020  4.2  Explicit typing.  Generic intrinsics.
!                  Remove needless FLOAT setting TSOFT.
!                  Replace loop 22, etc with IF (EXIT) GOTO ...
!                  Remove redundant TSTART.  Move label 8 inside block.
!                  Move initializations of EXIT,TOFT,TSNOW.
!                  Label 45 was 1000.  Test UZNEXT BEFORE loop 50.
!----------------------------------------------------------------------*
! Commons and constants
! Input common
!     SPEC.AL          FATAL,NEL,NLF,NM,NRAIN,NSTEP,PRI
!                      PALFA,TMAX,MELAST,METIME,UZNOW
!                      PINP(NRAIN),SD(NLF+1:NEL),TA(NM)
!                      BEXSM
!     SPEC.FR          BSOFT
! In+out common
!     SPEC.AL          UZNEXT
! Output common
!     SPEC.AL          P(NRAIN),PTOT(NRAIN)
      IMPLICIT NONE

! Locals, etc
      INTEGER             :: I, IEL, IFLAG, IOS
      DOUBLE PRECISION    :: TEND, TSNOW, TSOFT, UZTEST, PTOT(NRAIN)
      LOGICAL             :: EXITT, SMFLAG
      LOGICAL             :: PRDFIRST = .TRUE., PRDFIRST1 = .TRUE.
      LOGICAL             :: EPDFIRST = .TRUE., EPDFIRST1 = .TRUE.
      LOGICAL             :: TAHFIRST = .TRUE., TAHFIRST1 = .TRUE.
      LOGICAL             :: TALFIRST = .TRUE., TALFIRST1 = .TRUE.
      INTEGER             :: prdyear, prdmonth, prdday, prdhour, prdminute
      INTEGER             :: epdyear, epdmonth, epdday, epdhour, epdminute
      INTEGER             :: tahyear, tahmonth, tahday, tahhour, tahminute
      INTEGER             :: talyear, talmonth, talday, talhour, talminute
      DOUBLE PRECISION    :: prddate, epddate, tahdate, taldate
!----------------------------------------------------------------------*

! ----------------------------------------------------------------------
!  1.  COMPUTE EXPECTED TiMeSTEP
! ----------------------------------------------------------------------
      ! CALCULATE REDUCED TIMESTEP FOR SOFTSTART
      TSOFT = TMAX

      ! sb soft start not needed for hot start?
      IF (BHOTRD) BSOFT = .FALSE.

      IF (BSOFT .AND. NSTEP <= 102) TSOFT = TMAX * 0.05d0 * 1.03d0**NSTEP

      ! CALCULATE REDUCED TIMESTEP FOR SNOWMELT
      TSNOW = TMAX
      IF (BEXSM) THEN
         SMFLAG = .FALSE.
         DO I = 1, NM
            IF (TA(I) > 0.0d0) SMFLAG = .TRUE.
         END DO

         IF (SMFLAG) THEN
            snowmelt_check: DO IEL = total_no_links + 1, total_no_elements
               IF (SD(IEL) > 0.0d0) THEN
                  TSNOW = 0.5d0
                  EXIT snowmelt_check
               END IF
            END DO snowmelt_check
         END IF
      END IF

      ! SET TIMESTEP LENGTH
      UZNEXT = MIN(UZNEXT * (1.0d0 + PALFA), TSOFT, TSNOW)

      !**SB 07072020 reduce timestep if there are errors 1024,1030,1060
      IF (ISERROR2) THEN
         UZNEXT = MAX(0.0003d0, UZNEXT / 10.0d0)
      ELSEIF (ISERROR) THEN
         UZNEXT = MAX(0.0003d0, UZNEXT / 100.0d0)
      END IF

      ISERROR2 = .FALSE.
      ISERROR = .FALSE.

! ----------------------------------------------------------------------
!  2.  READ METEOROLOGICAL DATA AND REDUCE TMSTEP IF NECESSARY
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
!  2a.   check the start date is not before any met data occurs
! ----------------------------------------------------------------------
      IF (BMETDATES .AND. PRDFIRST1) THEN
         PRDFIRST1 = .FALSE.
         READ(prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            prdyear, prdmonth, prdday, prdhour, prdminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the precipitation time series file. ' // &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            WRITE(*, "('paused, type [enter] to continue')")
            READ (*, *)
            STOP
         END IF

         BACKSPACE(prd)
         prddate = HOUR_FROM_DATE(prdyear, prdmonth, prdday, prdhour, prdminute)

         ! check simulation start time plus precipitation time step length plus 0.01
         ! is greater than or equal to the first precipitation time series date.
         ! The 0.01 values is a bit arbitrary
         IF (tih + dtmet2 + 0.01d0 < prddate) THEN
            WRITE (*, '(A)') ' The precipitation data starts after the simulation start date. ' // &
               'Check the precipitation data dates and the start time of the simulation'
            WRITE(*, "('paused, type [enter] to continue')")
            READ (*, *)
            STOP
         END IF
      END IF

      IF (BMETDATES .AND. EPDFIRST1) THEN
         EPDFIRST1 = .FALSE.
         READ(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            epdyear, epdmonth, epdday, epdhour, epdminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the potential evaporation time series file. ' // &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            WRITE(*, "('paused, type [enter] to continue')")
            READ (*, *)
            STOP
         END IF

         BACKSPACE(epd)
         epddate = HOUR_FROM_DATE(epdyear, epdmonth, epdday, epdhour, epdminute)

         IF (tih + dtmet3 + 0.01d0 < epddate) THEN
            WRITE (*, '(A)') ' The potential evaporation data starts after the simulation start date. ' // &
               'Check the potential evaporation data dates and the start time of the simulation'
            WRITE(*, "('paused, type [enter] to continue')")
            READ (*, *)
            STOP
         END IF
      END IF

      IF (BMETDATES .AND. TAHFIRST1 .AND. ISTA) THEN
         TAHFIRST1 = .FALSE.
         READ(tah, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            tahyear, tahmonth, tahday, tahhour, tahminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the maximum temperature time series file. ' // &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            WRITE(*, "('paused, type [enter] to continue')")
            READ (*, *)
            STOP
         END IF

         BACKSPACE(tah)
         tahdate = HOUR_FROM_DATE(tahyear, tahmonth, tahday, tahhour, tahminute)

         IF (tih + dtmet3 + 0.01d0 < tahdate) THEN
            WRITE (*, '(A)') ' The maximum temperature data starts after the simulation start date. ' // &
               'Check the maximum temperature dates and the start time of the simulation'
            WRITE(*, "('paused, type [enter] to continue')")
            READ (*, *)
            STOP
         END IF
      END IF

      IF (BMETDATES .AND. TALFIRST1 .AND. ISTA) THEN
         TALFIRST1 = .FALSE.
         READ(tal, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
            talyear, talmonth, talday, talhour, talminute

         IF (ios /= 0) THEN
            WRITE (*, '(A)') ' Error reading the minimum temperature time series file. ' // &
               'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            WRITE(*, "('paused, type [enter] to continue')")
            READ (*, *)
            STOP
         END IF

         BACKSPACE(tal)
         taldate = HOUR_FROM_DATE(talyear, talmonth, talday, talhour, talminute)

         IF (tih + dtmet3 + 0.01d0 < taldate) THEN
            WRITE (*, '(A)') ' The minimum temperature data starts after the simulation start date. ' // &
               'Check the minimum temperature dates and the start time of the simulation'
            WRITE(*, "('paused, type [enter] to continue')")
            READ (*, *)
            STOP
         END IF
      END IF

! ----------------------------------------------------------------------
!  2b.   If the met data has dates then the first values can be ignored
!        if the simulation start date is after the met data start date
! ----------------------------------------------------------------------
      IF (BMETDATES .AND. PRDFIRST) THEN
         DO
            READ(prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               prdyear, prdmonth, prdday, prdhour, prdminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the precipitation time series file. ' // &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
               WRITE (*, '(A)') ' Check the format of the precipitation time series file ' // &
                  'and the end date is not before the start date of the simulation'
               WRITE(*, "('paused, type [enter] to continue')")
               READ (*, *)
               STOP
            END IF

            prddate = HOUR_FROM_DATE(prdyear, prdmonth, prdday, prdhour, prdminute)
            ! use the precipitation at this step if it is within 0.01 hour of the start date.
            ! Otherwise use the next precipitation file. The 0.01 values is a bit arbitrary
            IF (prddate + 0.01d0 > tih) THEN
               PRDFIRST = .FALSE.
               BACKSPACE(prd)
               EXIT
            END IF
         END DO
      END IF

      IF (BMETDATES .AND. EPDFIRST) THEN
         DO
            READ(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               epdyear, epdmonth, epdday, epdhour, epdminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the potential evaporation time series file. ' // &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               WRITE (*, '(A)') ' Check the format of the potential evaporation time series file ' // &
                  'and the end date is not before the start date of the simulation'
               WRITE(*, "('paused, type [enter] to continue')")
               READ (*, *)
               STOP
            END IF

            epddate = HOUR_FROM_DATE(epdyear, epdmonth, epdday, epdhour, epdminute)
            IF (epddate + 0.01d0 > tih) THEN
               EPDFIRST = .FALSE.
               BACKSPACE(epd)
               EXIT
            END IF
         END DO
      END IF

      IF (BMETDATES .AND. TAHFIRST .AND. ISTA) THEN
         DO
            READ(tah, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               tahyear, tahmonth, tahday, tahhour, tahminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the maximum temperature time series file. ' // &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               WRITE (*, '(A)') ' Check the format of the maximum daily temperature time series file ' // &
                  'and the end date is not before the start date of the simulation'
               WRITE(*, "('paused, type [enter] to continue')")
               READ (*, *)
               STOP
            END IF

            tahdate = HOUR_FROM_DATE(tahyear, tahmonth, tahday, tahhour, tahminute)
            IF (tahdate + 0.01d0 > tih) THEN
               TAHFIRST = .FALSE.
               BACKSPACE(tah)
               EXIT
            END IF
         END DO
      END IF

      IF (BMETDATES .AND. TALFIRST .AND. ISTA) THEN
         DO
            READ(tal, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) &
               talyear, talmonth, talday, talhour, talminute

            IF (ios /= 0) THEN
               WRITE (*, '(A)') ' Error reading the minimum daily temperature time series file. ' // &
                  'This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               WRITE (*, '(A)') ' Check the format of the minimum daily temperature time series file ' // &
                  'and the end date is not before the start date of the simulation'
               WRITE(*, "('paused, type [enter] to continue')")
               READ (*, *)
               STOP
            END IF

            taldate = HOUR_FROM_DATE(talyear, talmonth, talday, talhour, talminute)
            IF (taldate + 0.01d0 > tih) THEN
               TALFIRST = .FALSE.
               BACKSPACE(tal)
               EXIT
            END IF
         END DO
      END IF

! set period of validity of current data
      EXITT = .FALSE.

      timestep_reduction_loop: DO
         TEND = MIN(UZNOW + UZNEXT, METIME)

         ! store first period of precipitation using array slicing
         PTOT(1:NRAIN) = (TEND - UZNOW) * PINP(1:NRAIN)

         IF (EXITT) EXIT timestep_reduction_loop

         ! test if timestep reduction required without reading any prec. data
         DO I = 1, NRAIN
            IF (PTOT(I) > PMAX) THEN
               EXITT = .TRUE.
               UZNEXT = MIN(UZNEXT, PMAX / PINP(I))
            END IF
         END DO

         ! If we didn't trigger an exit condition, break the loop naturally
         IF (.NOT. EXITT) EXIT timestep_reduction_loop
      END DO timestep_reduction_loop

! read in prec. data if required, test for timestep reduction,
! and accumulate total prec.
      meteorological_loop: DO WHILE (.NOT. EXITT .AND. METIME < UZNOW + UZNEXT)
         IFLAG = 1
         CALL METIN(IFLAG)

         DO I = 1, NRAIN
            IF (PTOT(I) + (METIME - MELAST) * PINP(I) > PMAX) THEN
               EXITT = .TRUE.
               UZTEST = MELAST - UZNOW + (PMAX - PTOT(I)) / PINP(I)
               UZNEXT = MIN(UZNEXT, UZTEST)
            END IF
         END DO

         TEND = MIN(UZNOW + UZNEXT, METIME)

         ! Accumulate using array slicing
         PTOT(1:NRAIN) = PTOT(1:NRAIN) + (TEND - MELAST) * PINP(1:NRAIN)
      END DO meteorological_loop

! check for invalid timestep (could be a result of data errors)
      IF (UZNEXT < 5.0D-5) THEN
         WRITE(PPPRI, "(////'UZNEXT = ',G14.6, /' TSOFT = ',G14.6, /'MELAST = ',G14.6, " // &
            "/'METIME = ',G14.6 /, 'PREC.STN.   PINP        PTOT'/)") &
            UZNEXT, TSOFT, MELAST, METIME
         WRITE(PPPRI, "(4X,I4,2G14.6)") (I, PINP(I), PTOT(I), I = 1, NRAIN)
         CALL ERROR(FFFATAL, 1025, PPPRI, 0, 0, 'INVALID TIMESTEP')
      END IF

      ! calculate average value over timestep (& convert mm/h to m/s)
      DO IEL = 1, total_no_elements
         precip_m_per_s(IEL) = PTOT(NRAINC(IEL)) / UZNEXT / 3.6E6
      END DO

      ! read in breakpoint PE for this timestep (if required)
      IFLAG = 2
      CALL METIN(IFLAG)

   END SUBROUTINE TMSTEP

END MODULE rest
