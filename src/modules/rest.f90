!> summary: Miscellaneous run-control, meteorological input, and water-balance routines.
!>
!> This module collects legacy routines that do not naturally belong to one of
!> the process-specific modules. It writes final summary output, maintains the
!> column/link water-balance diagnostic, reads meteorological forcing as the run
!> advances, and computes the next model timestep subject to soft-start,
!> snowmelt, meteorological data boundaries, and runtime error-reduction flags.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 2008-12 | JE | 4.3.5F90 | Created during Fortran 90 conversion to collect `.F` routines without another natural module home. |
!> | 2026-03 | SB | 4.6 | Added date-aware meteorological files for precipitation, potential evaporation, and max/min temperature. |
!> @endhistory
MODULE rest
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

   LOGICAL :: FIRST_balwat=.TRUE. !! `.TRUE.` until `BALWAT` has initialised previous-storage state.
   DOUBLEPRECISION :: STORW_balwat(NELEE)=zero !! Previous water storage depth for each element/link used by `BALWAT` (m).
   DOUBLEPRECISION :: pinp(nvee+10)=zero       !! Current precipitation input by rain station (mm/hr).
   DOUBLEPRECISION :: METIME=zero              !! End time of the current precipitation/full-meteorological record window (h).
   DOUBLEPRECISION :: MELAST=zero              !! Start time of the current precipitation/full-meteorological record window (h).
   DOUBLEPRECISION :: EPTIME=zero              !! End time of the current potential-evaporation record window (h).


   PRIVATE
   PUBLIC :: BALWAT, TMSTEP, EXTRA_OUTPUT, &
      metime, melast, eptime, pinp
!          start_impact_window, end_impact_window, per_rain, mx_cnt_rain, cnt_rain !these here for AD only
CONTAINS

!> Writes end-of-run error and spatially averaged water-balance summaries.
!>
!> `extra_output` is called after the simulation loop has completed. It reports
!> accumulated flow, sediment, and contaminant error counters, prints the normal
!> completion line, and writes spatially averaged cumulative flux and final
!> storage totals to the `.pri` output.
!>
!> | Output group | Source variables | Units |
!> |:-------------|:-----------------|:------|
!> | Error counts | `FLERRC`, `SYERRC`, `CMERRC` | count by error number |
!> | Cumulative flux totals | `BALANC(7:12)` divided by `CAREA` | mm over catchment |
!> | Final storage totals | `BALANC(13:17)` divided by `CAREA` | mm over catchment |
   SUBROUTINE extra_output()
      INTEGER :: i
      DOUBLEPRECISION    :: car
      WRITE(PPPRI, 1400)
      DO 10 I = 0, 100
10    IF (FLERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 1000, FLERRC (I)
      DO 20 I = 0, 100
20    IF (SYERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 2000, SYERRC (I)
      DO 30 I = 0, 100
30    IF (CMERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 3000, CMERRC (I)
      WRITE(PPPRI, 1600)
1400  FORMAT(// 'Error message asummary'/)
1500  FORMAT('No. of occurences of error number ',I4,': ',I6)

1600  FORMAT(/ 'End of error message asummary')
!
      WRITE(PPPRI, '(////)')
      WRITE(PPPRI, 9900) UZNOW, NSTEP
!
      WRITE (6,'(A)') ' '

      WRITE (6,*) 'Normal completion of SHETRAN run'
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



!> Updates the cumulative water-balance error for each column or link.
!>
!> The routine computes the change in stored surface/subsurface water since the
!> previous call and compares it with net supplied depth over the last timestep:
!> precipitation, evaporation, subsurface exchange, well flow, overland flow, and
!> lateral subsurface advection. The residual is accumulated in `WBERR` as a
!> diagnostic depth in metres.
!>
!> `WBERR(iel)` is the cumulative water-balance error for element or link `iel`.
!> It is the extra depth of water, in metres, created during successive
!> timesteps. On the first call the routine initialises `WBERR` and the previous
!> storage array, but does not add a residual because no previous storage state
!> is available.
!>
!> The stored depth used by the balance is
!>
!> \[
!> S_{iel} =
!> \begin{cases}
!> ARXL_{iel}/CWIDTH_{iel}, & \text{for channel links},\\
!> HRF_{iel}-ZGRUND_{iel}, & \text{otherwise},
!> \end{cases}
!> + \sum_{k=NLYRBT(iel,1)}^{LL} \Delta z_{k,iel}\,\theta_{k,iel},
!> \]
!>
!> where \(\theta\) is `VSTHE`. The storage change is
!> \(\Delta S = S_{iel}-S^{old}_{iel}\).
!>
!> The supplied rate depth before timestep conversion is
!>
!> \[
!> I_{iel} =
!> PNETTO_{iel} - EEVAP_{iel} + QVSBF_{iel} - QVSWEL_{iel}
!> - \sum_k ERUZ_{iel,k}
!> + \frac{Q_{adv}}{AREA_{iel}},
!> \]
!>
!> with channel-bank exchange
!>
!> \[
!> Q_{adv} = -QBKF_{iel,1}-QBKF_{iel,2}
!> \]
!>
!> for channel links, and zero otherwise before face terms are added. For the
!> two paired face directions the code then adds
!>
!> \[
!> Q_{adv} \leftarrow Q_{adv}
!> - QOC_{iel,j} + QOC_{iel,j+2}
!> + \sum_k \left(QVSH_{j,k,iel}+QVSH_{j+2,k,iel}\right),
!> \quad j=1,2.
!> \]
!>
!> The timestep input depth is `DEPTHI = I * DTUZ`, and the diagnostic update is
!>
!> \[
!> WBERR_{iel} \leftarrow WBERR_{iel} + \Delta S - DEPTHI .
!> \]
!>
!> Main shared inputs are the active model dimensions and geometry (`LL`,
!> `NEL`, `NLYRBT`, `AREA`, `CWIDTH`, `DELTAZ`, `ZGRUND`), timestep and forcing
!> terms (`DTUZ`, `PNETTO`, `EEVAP`, `ERUZ`), storage and flow terms (`ARXL`,
!> `HRF`, `QBKF`, `QOC`, `QVSBF`, `QVSH`, `QVSWEL`, `VSTHE`), and element type
!> metadata from `ICMREF`.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-03 | RAH | 3.4.1 | Standard header, explicit declarations, extra comments, and first-pass storage initialisation. |
!> | 1995-02-20 | GP | 4.0 | Updated for the VSS module and revised subsurface flow variables. |
!> | 1997-02-17 | RAH | 4.1 | Swapped array subscripts for `QVSH`, `DELTAZ`, and `VSTHE`; renamed local counters. |
!> @endhistory
   SUBROUTINE BALWAT
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



!> Reads or interpolates meteorological forcing required by ET, interception, and snowmelt.
!>
!> `METIN` advances precipitation, potential evaporation, radiation, wind,
!> temperature, vapour pressure deficit, and time-varying vegetation/canopy
!> parameters as needed for the current simulation time. In date-aware mode,
!> [[tmstep]] first checks and positions the dated forcing files; `METIN` then
!> consumes the selected records and converts ISO-like date fields to SHETRAN
!> hours using the `hour_from_date` utility.
!>
!> The routine reads the meteorological data needed for the Penman-Monteith
!> evapotranspiration calculation, interception, and snowmelt. The manual
!> defines the controlling records as `ET2`, `ET4`, `ET6`, `ME2`/`ME3`,
!> `ME4`/`ME5`/`ME6`, `PR2`, and `EP2`:
!>
!> | Mode | Files and records | Code path |
!> |:-----|:------------------|:----------|
!> | `BMETAL=.FALSE.` | Full meteorological data in `MED`, updated every `DTMET` hours. If `NM=NRAIN`, rainfall and meteorological data share the same station distribution and are read together from `ME2`; optional measured PE is read from `ME3`. If `NM<NRAIN`, meteorological data are read from `ME4`/`ME5` and rainfall from `ME6`. | The routine reads `RN`, `U`, `TA`, `DEL`, `VPD`, optional `OBSPE`, and `PINP`. |
!> | `BMETAL=.TRUE.` | Separate precipitation `PRD` and potential-evaporation `EPD` files, updated every `DTMET2` and `DTMET3` hours respectively. Optional date-aware files include an ISO-8601-like first column when `BMETDATES=.TRUE.`. | `PINP` is read from `PR2`; potential evaporation `PEIN`/`OBSPE` is read from `EP2`. Optional max/min temperature files are read when `ISTA` is enabled. |
!>
!> The principal variables and units are:
!>
!> | Variable | Meaning | Input units | Internal use |
!> |:---------|:--------|:------------|:-------------|
!> | `ISITE` | Station identifier. | - | Read but not used for interpolation here. |
!> | `METIME` | Validity time of the current meteorological data. | h | Advanced by `DTMET`, `DTMET2`, or `DTMET3`. |
!> | `DTMET` | Full meteorological-data interval. | h | MED update interval. |
!> | `DTMET2` | Precipitation-data interval. | h | PRD update interval. |
!> | `DTMET3` | Potential-evaporation-data interval. | h | EPD update interval. |
!> | `PINP` | Precipitation. | mm/hr in MED; interval depth over `DTMET2` in PRD. | Stored as a rate in mm/hr for timestep accumulation in [[tmstep]]. |
!> | `OBSPE` | Measured potential evaporation/evapotranspiration. | mm/hr in MED; interval depth over `DTMET3` in EPD. | Stored as mm/s for ET calculations. |
!> | `RN` | Net radiation. | W/m^2 | Used by ET. |
!> | `U` | Wind speed. | m/s | Used by ET. |
!> | `TA` | Air temperature. | C | Used by ET and snowmelt. |
!> | `DEL` | Slope of saturation vapour pressure versus temperature. | mb/C | Used by ET. |
!> | `VPD` | Vapour pressure deficit. | mb | Used by ET. |
!> | `PA` | Atmospheric pressure. | mb | Read from MED but not used. |
!> | `IDATA` | Data-quality indicator. | - | Read from MED but not used. |
!>
!> For separate PRD/EPD files the manual gives precipitation and potential
!> evaporation as interval amounts. The code converts them to rates before
!> later timestep averaging:
!>
!> \[
!> PINP_i = \frac{PR2_i}{DTMET2},\qquad
!> PEIN_i = \frac{EP2_i}{DTMET3}.
!> \]
!>
!> `TMSTEP` later accumulates `PINP` over the model timestep and converts the
!> average precipitation to `precip_m_per_s` with
!> `PTOT / UZNEXT / 3.6E6`. For separate EPD input, `METIN` accumulates
!> potential evaporation over the current model timestep,
!>
!> \[
!> PETOT_i = \sum_m \Delta t_m\,PEIN_{i,m},
!> \]
!>
!> then stores the ET-module value as
!>
!> \[
!> OBSPE_i = \frac{PETOT_i}{UZNEXT\,3600},
!> \]
!>
!> in mm/s. When max/min temperature forcing is available, the air temperature
!> used at the end of the timestep is the simple average
!> \(TA_i=(TAHIGH_i+TALOW_i)/2\).
!>
!> If an input file ends, the first occurrence is reported to the `.pri` output;
!> remaining precipitation or PE values are set to zero, while missing optional
!> max/min temperatures default to 10 C. The legacy comment notes that
!> precipitation is averaged over the computational timestep elsewhere; that
!> averaging is performed by [[tmstep]].
!>
!> @note For dated PRD/EPD/TAH/TAL files the parsed dates are used for start-file
!> checks and initial positioning in [[tmstep]]. Within this routine the active
!> record windows are still advanced by `DTMET2` and `DTMET3`.
!> @endnote
!>
!> @history
!>
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1994-10-01 | RAH | 3.4.1 | Added legacy double-precision typing. |
!> | 1996-12-28 | RAH | 4.1 | Initialised `PELAST`; moved data from `SPEC.ET`; removed redundant interpolation argument. |
!> | 2026-03 | SB | 4.6 | Added optional date-aware meteorological input handling. |
!> @endhistory
   SUBROUTINE METIN (IFLAG)
! Input arguments

      INTEGER, INTENT(IN) :: IFLAG !! Read mode: `1` advances precipitation records; `2` advances potential evaporation and time-varying ET parameters.
! Locals, etc
!INTRINSIC MIN
      INTEGER :: I, IDATA, ISITE, K, NN
      DOUBLEPRECISION EPLAST, TCURR, TEND
      DOUBLEPRECISION PA (NVEE), PEIN (NVEE), PETOT (NVEE), per(nrain),tahight(nvee),talowt(nvee),tahigh(nvee),talow(nvee)
      logical :: firstnoprd, firstnoepd1, firstnoepd2
      logical :: firstnomet1, firstnomet2, firstnomet3
      logical :: firstnomet4, firstnomet5
      data firstnoprd / .true. /
      data firstnoepd1 / .true. /
      data firstnoepd2 / .true. /
      data firstnomet1 / .true. /
      data firstnomet2 / .true. /
      data firstnomet3 / .true. /
      data firstnomet4 / .true. /
      integer :: prdyear,prdmonth,prdday,prdhour,prdminute,prdsecond
      integer :: epdyear,epdmonth,epdday,epdhour,epdminute,epdsecond
      integer :: tahyear,tahmonth,tahday,tahhour,tahminute,tahsecond
      integer :: talyear,talmonth,talday,talhour,talminute,talsecond
      character(len=1000000) :: tmp
      integer :: ios
      DOUBLEPRECISION :: prddate,epddate,tahdate,taldate


      data firstnomet5 / .true. /
!----------------------------------------------------------------------*


      IF (.NOT.BMETAL) GOTO 40
!
! READ PREC. & OBSERVED POT. EVAPOTRANSPIRATION BREAKPOINT FORMAT FILES
!-----------------------------------------------------------------------
!
! PRECIPITATION
! read only one line of file (unless hotstarted run)

      IF (IFLAG.EQ.1) THEN
!    5   READ (PRD,*,END=280) I1, I2, I3, I4, I5, (PINP(I),I=1,NRAIN)
!        MELAST = METIME
!        METIME = HOUR(I1,I2,I3,I4,I5) - TIH
         do
            if (BMETDATES) then
               read(prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', iostat=ios) prdyear,prdmonth,prdday,prdhour,prdminute,prdsecond, tmp
               if (ios>0) then
                  write (*,'(A,I0,A)') ' Error reading the precipitation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NRAIN, ' values on each row'
                  write(*,'(''paused, type [enter] to continue'')')
                  read (*,*)
                  stop
               endif
               if (ios<0) then
                  if (firstnoprd) then
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
                     WRITE(PPPRI, '(A18)') 'Finish of prd data'
                     WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, * )
                     firstnoprd = .false.
                  endif
                  pinp (1:nrain) = zero
               endif
               prddate = HOUR_FROM_DATE( prdyear,prdmonth,prdday,prdhour,prdminute)
!        write(*,*) prddate
               READ (tmp, *,iostat=ios) (PINP (I), I = 1, NRAIN)
               if (ios>0) then
                  write (*,'(A,I0,A)') ' Error reading the precipitation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NRAIN, ' values on each row'
                  write(*,'(''paused, type [enter] to continue'')')
                  read (*,*)
                  stop
               endif

            else
               READ (PRD, *, iostat=ios) (PINP (I), I = 1, NRAIN)
               if (ios>0) then
                  write (*,'(A,I0,A)') ' Error reading the precipitation time series file. This should have ',NRAIN, ' values on each row with no dates in the first column (see ET1)'
                  write(*,'(''paused, type [enter] to continue'')')
                  read (*,*)
                  stop
               endif
               if (ios<0) then
                  if (firstnoprd) then
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
                     WRITE(PPPRI, '(A18)') 'Finish of prd data'
                     WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, * )
                     WRITE(PPPRI, * )
                     firstnoprd = .false.
                  endif
                  pinp (1:nrain) = zero
               endif
            endif
            pinp (1:nrain) = pinp (1:nrain) / dtmet2
            MELAST = METIME
            METIME = METIME+dtmet2
!
            IF (.not.(BHOTRD.AND.METIME.LT.BHOTTI)) exit
         enddo
!
      ELSE
! POT. EVAP and TEMPERATURE DATA READ PART 1

! first check for hotstarted run
         IF (BHOTRD.AND.EPTIME.LT.BHOTTI) THEN

            ! do loop which is exited if (.not.(BHOTRD.AND.EPTIME.LT.BHOTTI  ))
            do

               ! epd and temperature files have dates
               if (BMETDATES) then

                  read(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', iostat=ios) epdyear,epdmonth,epdday,epdhour,epdminute,epdsecond, tmp

                  if (ios>0) then
                     write (*,'(A,I0,A)') ' Error reading the potential evaporation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                     write(*,'(''paused, type [enter] to continue'')')
                     read (*,*)
                     stop
                  endif
                  if (ios<0) then
                     if (firstnoepd2) then
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
                        WRITE(PPPRI, '(A18)') 'Finish of epd data'
                        WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        firstnoepd2 = .false.
                     endif
                     pein (1:nm) = zero
                  endif
                  epddate = HOUR_FROM_DATE( epdyear,epdmonth,epdday,epdhour,epdminute)

                  READ (tmp, *,iostat=ios) (PEIN (I), I = 1, NM)
                  if (ios>0) then
                     write (*,'(A,I0,A)') ' Error reading the potential evaporation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                     write(*,'(''paused, type [enter] to continue'')')
                     read (*,*)
                     stop
                  endif


                  if (ista) then
                     read(TAH, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', iostat=ios) tahyear,tahmonth,tahday,tahhour,tahminute,tahsecond, tmp
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the max temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     if (ios<0) tahigh(1:nm) = 10.0

                     READ (tmp, *,iostat=ios) (tahigh (I), I = 1, NM)
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the max temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     tahdate = HOUR_FROM_DATE( tahyear,tahmonth,tahday,tahhour,tahminute)

                  endif

                  if (ista) then
                     read(TAL, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', iostat=ios) talyear,talmonth,talday,talhour,talminute,talsecond, tmp
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the min temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     if (ios<0) talow(1:nm) = 10.0

                     READ (tmp, *,iostat=ios) (talow (I), I = 1, NM)
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the min temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     taldate = HOUR_FROM_DATE(talyear,talmonth,talday,talhour,talminute)
                  endif

                  pein(1:nm) = pein(1:nm) / dtmet3

                  EPLAST = EPTIME
                  EPTIME = EPTIME+dtmet3

                  IF (.not.(BHOTRD.AND.EPTIME.LT.BHOTTI)) exit
                  !****

                  ! epd and temperature files DO NOT have dates
               else

                  READ (EPD, *, iostat=ios) (PEIN (I), I = 1, NM)

                  if (ios>0) then
                     write (*,'(A,I0,A)') ' Error reading the potential evaporation time series file. This should have ',NM, ' values on each row with no dates in the first column (see ET1)'
                     write(*,'(''paused, type [enter] to continue'')')
                     read (*,*)
                     stop
                  endif
                  if (ios<0) then
                     if (firstnoepd1) then
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
                        WRITE(PPPRI, '(A18)') 'Finish of epd data'
                        WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        firstnoepd1 = .false.
                     endif
                     pein (1:nm) = zero
                  endif

                  if (ista) then
                     READ (TAH, *, iostat=ios) (tahigh (I), I = 1, NM)
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the max temperature time series file. This should have ',NM, ' values on each row with no dates in the first column (see ET1)'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     if (ios<0) tahigh(1:nm) = 10.0
                  endif

                  if (ista) then
                     READ (TAL, *, iostat=ios) (talow (I), I = 1, NM)
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the min temperature time series file. This should have ',NM, ' values on each row with no dates in the first column (see ET1)'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     if (ios<0) talow(1:nm) = 10.0
                  endif

                  pein(1:nm) = pein(1:nm) / dtmet3

                  EPLAST = EPTIME
                  EPTIME = EPTIME+dtmet3

                  IF (.not.(BHOTRD.AND.EPTIME.LT.BHOTTI)) exit
                  !****

                  ! end of if else epd and temperature files have dates
               endif


            enddo


         ENDIF
! end of check for hotstarted run


! calculate average PE value over computational timestep
         TEND = MIN (UZNOW + UZNEXT, EPTIME)
         PETOT (1:NM) = (TEND-UZNOW) * PEIN (1:NM)

! POT. EVAP and TEMPERATURE DATA READ PART 2

         !check if it is time to read in PET data
         IF (EPTIME.LT.UZNOW + UZNEXT) THEN

            ! do loop which is exited if ((.not.(EPTIME.LT.UZNOW + UZNEXT))
            do
!     read(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', iostat=ios, END = 285) metyear,metmonth,metday,methour,metminute,metsecond, tmp
!     READ (tmp, *) (PEIN (I), I = 1, NM)

               ! epd and temperature files have dates
               if (BMETDATES) then
                  read(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', iostat=ios) epdyear,epdmonth,epdday,epdhour,epdminute,epdsecond, tmp

                  if (ios>0) then
                     write (*,'(A,I0,A)') ' Error reading the potential evaporation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                     write(*,'(''paused, type [enter] to continue'')')
                     read (*,*)
                     stop
                  endif
                  if (ios<0) then
                     if (firstnoepd2) then
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
                        WRITE(PPPRI, '(A18)') 'Finish of epd data'
                        WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        firstnoepd2 = .false.
                     endif
                     pein (1:nm) = zero
                  endif
                  epddate = HOUR_FROM_DATE( epdyear,epdmonth,epdday,epdhour,epdminute)

                  READ (tmp, *,iostat=ios) (PEIN (I), I = 1, NM)
                  if (ios>0) then
                     write (*,'(A,I0,A)') ' Error reading the potential evaporation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                     write(*,'(''paused, type [enter] to continue'')')
                     read (*,*)
                     stop
                  endif

                  if (ista) then
                     read(TAH, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', iostat=ios) tahyear,tahmonth,tahday,tahhour,tahminute,tahsecond, tmp
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the max temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     if (ios<0) tahigh(1:nm) = 10.0

                     READ (tmp, *,iostat=ios) (tahigh (I), I = 1, NM)
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the max temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     tahdate = HOUR_FROM_DATE( tahyear,tahmonth,tahday,tahhour,tahminute)

                  endif

                  if (ista) then
                     read(TAL, '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a)', iostat=ios) talyear,talmonth,talday,talhour,talminute,talsecond, tmp
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the min temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     if (ios<0) talow(1:nm) = 10.0

                     READ (tmp, *,iostat=ios) (talow (I), I = 1, NM)
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the min temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 followed by ',NM, ' values on each row'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     taldate = HOUR_FROM_DATE(talyear,talmonth,talday,talhour,talminute)


                  endif

                  pein (1:nm) = pein (1:nm) / dtmet3

                  EPLAST = EPTIME
                  EPTIME = EPTIME+dtmet3
                  TEND = MIN (UZNOW + UZNEXT, EPTIME)
                  PETOT (1:nm) = PETOT (1:nm) + (TEND-EPLAST) * PEIN (1:nm)
                  IF (.not.(EPTIME.LT.UZNOW + UZNEXT)) exit
                  !****


                  ! epd and temperature files DO NOT have dates
               else
                  READ (EPD, *, iostat=ios) (PEIN (I), I = 1, NM)
                  if (ios>0) then
                     write (*,'(A,I0,A)') ' Error reading the potential evaporation time series file. This should have ',NM, ' values on each row with no dates in the first column (see ET1)'
                     write(*,'(''paused, type [enter] to continue'')')
                     read (*,*)
                     stop
                  endif
                  if (ios<0) then
                     if (firstnoepd2) then
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
                        WRITE(PPPRI, '(A18)') 'Finish of epd data'
                        WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        WRITE(PPPRI, * )
                        firstnoepd2 = .false.
                     endif
                     pein (1:nm) = zero
                  endif


                  if (ista) then
                     READ (TAH, *, iostat=ios) (tahigh (I), I = 1, NM)
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the max temperature time series file. This should have ',NM, ' values on each row with no dates in the first column (see ET1)'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     if (ios<0) tahigh(1:nm) = 10.0
                  endif

                  if (ista) then
                     READ (TAL, *, iostat=ios) (talow (I), I = 1, NM)
                     if (ios>0) then
                        write (*,'(A,I0,A)') ' Error reading the min temperature time series file. This should have ',NM, ' values on each row with no dates in the first column (see ET1)'
                        write(*,'(''paused, type [enter] to continue'')')
                        read (*,*)
                        stop
                     endif
                     if (ios<0) talow(1:nm) = 10.0
                  endif

                  pein (1:nm) = pein (1:nm) / dtmet3

                  EPLAST = EPTIME
                  EPTIME = EPTIME+dtmet3
                  TEND = MIN (UZNOW + UZNEXT, EPTIME)
                  PETOT (1:nm) = PETOT (1:nm) + (TEND-EPLAST) * PEIN (1:nm)
                  IF (.not.(EPTIME.LT.UZNOW + UZNEXT)) exit
                  !****

                  ! end of if else epd and temperature files have dates
               endif

            enddo

         ENDIF
         OBSPE (1:nm) = PETOT (1:nm) / UZNEXT / 3600.
! for simplicity the temperature used is the value at the end of the timestep
         ta (1:nm) = (tahigh (1:nm) +  talow (1:nm) )/2.0

      ENDIF
!
! PRINT OUT INPUT DATA
!
      IF (BMETP) THEN
         WRITE(PPPRI, 30) METIME
30       FORMAT   (//1X, 'MET DATA -  TIME :',F8.2 / &
         &   ' STATION           RAINFALL      POT. EVAP.(MM/HR)')
         DO 35 I = 1, NM
            WRITE(PPPRI, 32) I, PINP (I), PEIN (I)
32          FORMAT    (4X,I2,9X,F10.3,9X,F10.3)
35       END DO
      ENDIF
!
      GOTO 190
!
! READ ALL MET. DATA IN FIXED TIME INTERVAL (USUALLY HOURLY) FORMAT
!------------------------------------------------------------------
!
!^^^^^^^^^              GP 29/9/92
40    IF (IFLAG.EQ.2) RETURN
!^^^^^^^^^
      IF (NRAIN.NE.NM) GOTO 100
!
!-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE EQUAL
!
      IF (BMETP) WRITE(PPPRI, 50)
50    FORMAT (//1X, 'MET DATA - SITE    TIME      RAINFALL    NET RADN', &
      &       4X, &
      & &
      &'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
!
!-----LOOP ON NUMBER OF MET SITES
!
55    MELAST = METIME

      METIME = METIME+DTMET
      DO 90 I = 1, NM
         READ (MED, 60, END = 287) ISITE, NN, PINP (I), RN (I), U (I), &
            PA (I), TA (I), DEL (I), VPD (I), IDATA

         goto 288
287      if (firstnomet1) then
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
            WRITE(PPPRI, '(A18)') 'Finish of met data'
            WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            firstnomet1 = .false.
         endif
         isite = 1
         nn = 1
         pinp (i) = zero
         rn (i) = zero
         u (i) = zero
         pa (i) = zero
         ta (i) = 10.0d0
         del (i) = one
         vpd (i) = three

         idata = 1000
288      IF (BMETP) WRITE(PPPRI, 70) ISITE, METIME, PINP (I), RN (I), &
            U (I), TA (I), DEL (I), VPD (I)
60       FORMAT   (2I6, 4G12.6, /, 12X, 3G12.6, I12)
70       FORMAT   ('0', 8X, I6, F8.2, 5X, 2(3F12.6,'  NOT_USED  '))
         IF (MEASPE (I) .EQ.0) GOTO 90
!
! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
!
         READ (MED, 80, END = 289) OBSPE (I)
80       FORMAT   (12X, G12.6)

         goto 290
289      if (firstnomet2) then
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
            WRITE(PPPRI, '(A18)') 'Finish of met data'
            WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            firstnomet2 = .false.
         endif


         obspe (i) = 0.0
!
! CONVERT TO MM/S
!
290      OBSPE (I) = OBSPE (I) / 3600.
90    END DO
!
! READ TO START SIMULATION TIME, IF HOTSTART
!
      IF (BHOTRD.AND.METIME.LT.BHOTTI) GOTO 55
!
      GOTO 190
!
!-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE UNEQUAL
!
100   IF (BMETP) WRITE(PPPRI, 110)
110   FORMAT (//1X, 'MET DATA - SITE    TIME      NET RADN', 4X, &
      & &
      &'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
!
!-----LOOP ON NUMBER OF MET SITES
!
115   MELAST = METIME
      METIME = METIME+DTMET
      DO 140 I = 1, NM
         READ (MED, 120, END = 291) ISITE, NN, RN (I), U (I), PA (I), &
            TA (I), DEL (I), VPD (I), IDATA

         goto 292
291      if (firstnomet3) then
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
            WRITE(PPPRI, '(A18)') 'Finish of met data'
            WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            firstnomet3 = .false.
         endif
         isite = 1
         nn = 1
         rn (i) = zero
         u (i) = zero
         pa (i) = zero
         ta (i) = 10.0d0
         del (i) = one
         vpd (i) = three

         idata = 1000
292      IF (BMETP) WRITE(PPPRI, 130) ISITE, METIME, RN (I), U (I), &
            TA (I), DEL (I), VPD (I)
120      FORMAT   (2I6, 12X, 3G12.6, /, 12X, 3G12.6, I12)
130      FORMAT   ('0', 8X, I6, F8.2, 5X, 2(2F12.6,'  NOT_USED  ':F12.6))
         IF (MEASPE (I) .EQ.0) GOTO 140
!
! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
!
         READ (MED, 80, END = 293) OBSPE (I)

         goto 294
293      if (firstnomet4) then
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
            WRITE(PPPRI, '(A18)') 'Finish of met data'
            WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            firstnomet4 = .false.
         endif

         obspe (i) = 0.0
!
! CONVERT TO MM/S
!
294      OBSPE (I) = OBSPE (I) / 3600.
140   END DO
      IF (BMETP) WRITE(PPPRI, 150)

150   FORMAT (//1X, 'RAIN DATA - SITE    TIME      RAINFALL         IDATA')
!
!-----LOOP ON NUMBER OF RAIN SITES
!
      DO 180 I = 1, NRAIN
         READ (MED, 160, END = 295) ISITE, NN, PINP (I), IDATA

         goto 296
295      if (firstnomet5) then
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, '(A6,g12.4,a8)') 'Time = ', uznow, ' Hours.'
            WRITE(PPPRI, '(A18)') 'Finish of met data'
            WRITE(PPPRI, '(A33)') 'All remaining values will be zero'
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            WRITE(PPPRI, * )
            firstnomet5 = .false.
         endif

         pinp (i) = 0.0
296      IF (BMETP) WRITE(PPPRI, 170) ISITE, METIME, PINP (I)
160      FORMAT   (2I6, G12.6, 24X, I12)
170      FORMAT   ('0', 9X, I6, F8.2, 5X, F12.6, '  NOT_USED  ')
180   END DO
!
! READ TO SIMULATION START TIME, IF HOTSTART
!
      IF (BHOTRD.AND.METIME.LT.BHOTTI) GOTO 115
!
!--------------------------------------------
!     CHECK TIME-VARYING MODEL PARAMETERS
!--------------------------------------------
!
190   TCURR = TIMEUZ
      DO 270 K = 1, NV
! sb 04032025 for dynamically allocated arrays use NV not NVEE
         IF (MODECS (K) .NE.0) CALL TERPO1 (CSTCAP, TCURR, RELCST, TIMCST, NCTCST, CSTCA1, NV, K)
         IF (MODEPL (K) .NE.0) CALL TERPO1 (PLAI, TCURR, RELPLA, TIMPLA, NCTPLA, PLAI1, NV, K)
         IF (MODECL (K) .NE.0) CALL TERPO1 (CLAI, TCURR, RELCLA, TIMCLA, NCTCLA, CLAI1, NV, K)
         IF (MODEVH (K) .NE.0) CALL TERPO1 (VHT, TCURR, RELVHT, TIMVHT,  NCTVHT, VHT1, NV, K)
270   END DO
!
      RETURN
      STOP
   END SUBROUTINE METIN



!> Computes the next simulation timestep and reads any required meteorological data.
!>
!> The timestep is limited by soft-start growth, snowmelt conditions, forcing-data
!> record boundaries, maximum timestep controls, and runtime reductions triggered
!> by selected flow errors. This routine is the main point where meteorological
!> file timing and hydrological stability controls meet before the next model
!> step is taken.
!>
!> The candidate timestep is first reduced by these controls:
!>
!> | Control | Code expression | Effect |
!> |:--------|:----------------|:-------|
!> | Growth from previous step | `UZNEXT*(1+PALFA)` | Prevents abrupt timestep expansion. |
!> | Soft start | `TMAX*0.05*1.03**NSTEP` for the first 102 steps when `BSOFT` is true | Starts the run with smaller steps; disabled for hot starts. |
!> | Snowmelt | `0.5` h when snow is present and any met station has `TA>0` | Limits melt-period steps. |
!> | Runtime errors | `UZNEXT/10` or `UZNEXT/100`, lower-bounded by `0.0003` h | Retries after selected flow errors. |
!>
!> For date-aware forcing (`BMETDATES`) the first call checks that PRD, EPD, and
!> optional TAH/TAL records do not start after the simulation start date. It also
!> skips older records until the first record whose date is within about
!> `0.01` h of `TIH` or later, then backspaces so `METIN` can read that record.
!>
!> Precipitation is accumulated over the candidate timestep by splitting at
!> meteorological record boundaries:
!>
!> \[
!> PTOT_i = \sum_m \Delta t_m\,PINP_{i,m}.
!> \]
!>
!> If any accumulated station total would exceed `PMAX`, the timestep is reduced
!> to the crossing time. The final element precipitation rate is then
!>
!> \[
!> precip\_m\_per\_s(e) =
!> \frac{PTOT_{NRAINC(e)}}{UZNEXT\,3.6\times10^6}.
!> \]
!>
!> Finally `METIN(2)` reads or interpolates PE and time-varying vegetation/canopy
!> parameters needed for the timestep.
!>
!> @history
!> | Date | Author | Version | Description |
!> |:-----|:-------|:--------|:------------|
!> | 1993-07 | GP | 3.4 | Reworked `UZNEXT` algorithm and added soft-start controls. |
!> | 1994-10-03 | RAH | 3.4.1 | Added legacy double-precision typing. |
!> | 1996-07-17 | GP | 4.0 | Limited timestep during snowmelt. |
!> | 1998-10-20 | RAH | 4.2 | Reworked control flow and initialisation. |
!> | 2020-07-07 | SB | - | Added timestep reduction after selected runtime errors. |
!> | 2026-03 | SB | 4.6 | Added date-aware checks for meteorological forcing files. |
!> @endhistory
   SUBROUTINE TMSTEP
! Locals, etc
!INTRINSIC MIN
      INTEGER :: I, IEL, IFLAG
      DOUBLEPRECISION TEND, TSNOW, TSOFT, UZTEST, PTOT(nrain)
      LOGICAL :: exitt, SMFLAG, iscycle, jumpto45, first
      LOGICAL :: PRDFIRST=.true.,PRDFIRST1=.true.
      LOGICAL :: EPDFIRST=.true., EPDFIRST1=.true.
      LOGICAL :: TAHFIRST=.true.,TAHFIRST1=.true.
      LOGICAL :: TALFIRST=.true.,TALFIRST1=.true.
      integer :: prdyear,prdmonth,prdday,prdhour,prdminute,prdsecond
      integer :: epdyear,epdmonth,epdday,epdhour,epdminute,epdsecond
      integer :: tahyear,tahmonth,tahday,tahhour,tahminute,tahsecond
      integer :: talyear,talmonth,talday,talhour,talminute,talsecond
      integer :: ios
      DOUBLEPRECISION :: prddate,epddate,tahdate,taldate
!----------------------------------------------------------------------*
! ----------------------------------------------------------------------
!  1.  COMPUTE EXPECTED TiMeSTEP
! ----------------------------------------------------------------------
! CALCULATE REDUCED TIMESTEP FOR SOFTSTART
      TSOFT = TMAX

      jumpto45 = .FALSE.

!sb soft start not needed for hot start?
      IF (BHOTRD) BSOFT=.false.

      IF (BSOFT.AND.NSTEP.LE.102) TSOFT = TMAX * 0.05d0 * 1.03d0**NSTEP
! CALCULATE REDUCED TIMESTEP FOR SNOWMELT
      TSNOW = TMAX
      IF(BEXSM) THEN
         SMFLAG = .FALSE.
         DO 5 I = 1, NM
            IF (GTZERO(TA(I))) SMFLAG = .TRUE.
5        ENDDO
         IF(SMFLAG) THEN
            iscycle = .FALSE.
            DO 7 IEL = total_no_links + 1, total_no_elements
               IF(iscycle) CYCLE
               IF (GTZERO(SD(IEL))) THEN
                  TSNOW = 0.5
                  iscycle = .TRUE. ! GOTO 8
                  !                    vvvvvv
               ENDIF
7           ENDDO
            ! 8 CONTINUE
         ENDIF
      ENDIF
! SET TIMESTEP LENGTH
      UZNEXT = MIN (UZNEXT * (1.0 + PALFA), TSOFT, TSNOW)

! SB 07072020 reduce timestep if there are errors 1024,1030,1060.
      IF (ISERROR2) THEN
         UZNEXT = max(0.0003,uznext/10.0)
      ELSEIF (ISERROR) THEN
         UZNEXT = max(0.0003,uznext/100.0)
      ENDIF
      ISERROR2 = .FALSE.
      ISERROR = .FALSE.

! ----------------------------------------------------------------------
!  2.  READ METEOROLOGICAL DATA AND REDUCE TMSTEP IF NECESSARY
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
!  2a.   check the start date is not before any met data occurs
! ----------------------------------------------------------------------
      If  (BMETDATES.and.PRDFIRST1) then
         PRDFIRST1 = .false.
         read(prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) prdyear,prdmonth,prdday,prdhour,prdminute
         if (ios/=0) then
            write (*,'(A)') ' Error reading the precipitation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
         endif
         backspace(prd)
         prddate = HOUR_FROM_DATE( prdyear,prdmonth,prdday,prdhour,prdminute)
         ! check simulation start timne plus precipitation time step length plus 0.01 is greater than or equal to the first precipitation time series date. The 0.01 values is a bit arbitrary
         if (tih+dtmet2+0.01.lt.prddate) then
            write (*,'(A)') ' The precipitation data starts after the simulation start date. Check the precipitation data dates and the start time of the simulation'
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
         endif
      endif
      If  (BMETDATES.and.EPDFIRST1) then
         EPDFIRST1 = .false.
         read(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios)  epdyear,epdmonth,epdday,epdhour,epdminute
         if (ios/=0) then
            write (*,'(A)') ' Error reading the potential evaporation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
         endif
         backspace(epd)
         epddate = HOUR_FROM_DATE( epdyear,epdmonth,epdday,epdhour,epdminute)
         if (tih+dtmet3+0.01.lt.epddate) then
            write (*,'(A)') ' The potential evaporation data starts after the simulation start date. Check the potential evaporation data dates and the start time of the simulation'
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
         endif
      endif
      If  (BMETDATES.and.TAHFIRST1.and.ISTA) then
         TAHFIRST1 = .false.
         read(tah, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios)  tahyear,tahmonth,tahday,tahhour,tahminute
         if (ios/=0) then
            write (*,'(A)') ' Error reading the maximum temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
         endif
         backspace(tah)
         tahdate = HOUR_FROM_DATE(tahyear,tahmonth,tahday,tahhour,tahminute)
         if (tih+dtmet3+0.01.lt.tahdate) then
            write (*,'(A)') ' The maximum temperature data starts after the simulation start date. Check the maximum temperature dates and the start time of the simulation'
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
         endif
      endif
      If  (BMETDATES.and.TALFIRST1.and.ISTA) then
         TALFIRST1 = .false.
         read(tal, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios)  talyear,talmonth,talday,talhour,talminute
         if (ios/=0) then
            write (*,'(A)') ' Error reading the minimum temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
         endif
         backspace(tal)
         taldate = HOUR_FROM_DATE(talyear,talmonth,talday,talhour,talminute)
         if (tih+dtmet3+0.01.lt.taldate) then
            write (*,'(A)') ' The minimum temperature data starts after the simulation start date. Check the minimum temperature dates and the start time of the simulation'
            write(*,'(''paused, type [enter] to continue'')')
            read (*,*)
            stop
         endif
      endif

! ----------------------------------------------------------------------
!  2b.   If the met data has dates then the first values can be ignored if the simulation start date is after the met data start date
! ----------------------------------------------------------------------
      If  (BMETDATES.and.PRDFIRST) then
         do
            read(prd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) prdyear,prdmonth,prdday,prdhour,prdminute
            if (ios/=0) then
               write (*,'(A)') ' Error reading the precipitation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00'
               write (*,'(A)') ' Check the format of the precipitation time series file and the end date of the time series is not before the start date of the simulation'
               write(*,'(''paused, type [enter] to continue'')')
               read (*,*)
               stop
            endif

            prddate = HOUR_FROM_DATE( prdyear,prdmonth,prdday,prdhour,prdminute)
!       write(*,*) prddate
! use the precipitation at this step if it is within 0.01 hour of the start date. Otherwise use the next precipitation file. The 0.01 values is a bit arbitrary
            if (prddate + 0.01.gt.tih) then
               PRDFIRST = .false.
               backspace(prd)
               exit
            endif
         enddo
      endif
      If  (BMETDATES.and.EPDFIRST) then
         do
            read(epd, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) epdyear,epdmonth,epdday,epdhour,epdminute
            if (ios/=0) then
               write (*,'(A)') ' Error reading the potential evaporation time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               write (*,'(A)') ' Check the format of the potential evaporation time series file and the end date of the time series is not before the start date of the simulation'
               write(*,'(''paused, type [enter] to continue'')')
               read (*,*)
               stop
            endif
            epddate = HOUR_FROM_DATE( epdyear,epdmonth,epdday,epdhour,epdminute)
!       write(*,*) epddate
            if (epddate + 0.01.gt.tih) then
               EPDFIRST = .false.
               backspace(epd)
               exit
            endif
         enddo
      endif
      If  (BMETDATES.and.TAHFIRST.and.ISTA) then
         do
            read(tah, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) tahyear,tahmonth,tahday,tahhour,tahminute
            if (ios/=0) then
               write (*,'(A)') ' Error reading the maximum  temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               write (*,'(A)') ' Check the format of the maximum daily temperature time series file and the end date of the time series is not before the start date of the simulation'
               write(*,'(''paused, type [enter] to continue'')')
               read (*,*)
               stop
            endif
            tahdate = HOUR_FROM_DATE( tahyear,tahmonth,tahday,tahhour,tahminute)
!       write(*,*) tahdate
            if (tahdate + 0.01.gt.tih) then
               TAHFIRST = .false.
               backspace(tah)
               exit
            endif
         enddo
      endif
      If  (BMETDATES.and.TALFIRST.and.ISTA) then
         do
            read(tal, '(i4,1x,i2,1x,i2,1x,i2,1x,i2)', iostat=ios) talyear,talmonth,talday,talhour,talminute
            if (ios/=0) then
               write (*,'(A)') ' Error reading the minimum daily temperature time series file. This should have the date in the iso 8601 format e.g 1980-01-01T00:00:00 '
               write (*,'(A)') ' Check the format of the minimum daily temperature time series file and the end date of the time series is not before the start date of the simulation'
               write(*,'(''paused, type [enter] to continue'')')
               read (*,*)
               stop
            endif
            taldate = HOUR_FROM_DATE( talyear,talmonth,talday,talhour,talminute)
!       write(*,*) taldate
            if (taldate + 0.01.gt.tih) then
               TALFIRST = .false.
               backspace(tal)
               exit
            endif
         enddo
      endif



! set period of validity of current data
      exitt = .FALSE.
      first = .TRUE.
      DO WHILE((first .OR. exitt) .AND. .NOT.jumpto45)
         first = .FALSE.
9        TEND = MIN (UZNOW + UZNEXT, METIME)
         ! store first period of precipitation
         DO I = 1, NRAIN
            PTOT (I) = (TEND-UZNOW) * PINP (I)
         ENDDO
         IF(exitt) THEN
            jumpto45=.TRUE. !GOTO 45
            CYCLE  !EXIT
         ENDIF
         !                  vvvvvvv
         ! test if timestep reduction required without reading any prec. data
         DO I = 1, NRAIN
            IF (PTOT (I) .GT.PMAX) THEN
               exitt = .TRUE.
               UZNEXT = MIN (UZNEXT, PMAX / PINP (I) )
            ENDIF
         ENDDO
      ENDDO
!IF(exitt) GOTO 9
!               ^^^^^^
! read in prec. data if required, test for timestep reduction,
! and accumulate total prec.

      DO WHILE(.NOT.jumpto45 .AND. .NOT. exitt .AND. METIME.LT.UZNOW + UZNEXT)
!25 IF (METIME.LT.UZNOW + UZNEXT) THEN
         IFLAG = 1
         CALL METIN (IFLAG)
         DO 30 I = 1, NRAIN
            IF (PTOT (I) + (METIME-MELAST) * PINP (I) .GT.PMAX) THEN
               exitt = .TRUE.
               UZTEST = MELAST - UZNOW + (PMAX - PTOT (I) ) / PINP (I)
               UZNEXT = MIN (UZNEXT, UZTEST)
            ENDIF
30       ENDDO
         TEND = MIN (UZNOW + UZNEXT, METIME)
         DO I = 1, NRAIN
            PTOT (I) = PTOT (I) + (TEND-MELAST) * PINP (I)
         ENDDO
!   IF (.NOT.exitt) GOTO 25
!ENDIF
      ENDDO
! check for invalid timestep (could be a result of data errors)


      IF (UZNEXT.LT.5.0D-5) THEN  !45
         WRITE(PPPRI, 9060) UZNEXT, TSOFT, MELAST, METIME
         WRITE(PPPRI, 9070) (I, PINP (I), PTOT (I), I = 1, NRAIN)
         CALL ERROR(FFFATAL, 1025, PPPRI, 0, 0, 'INVALID TIMESTEP')
      ENDIF
      !                           vvvvvvvvv
      ! calculate average value over timestep (& convert mm/h to m/s)
      !DO 50 I = 1, NRAIN
      !   precip_m_per_s(I) = PTOT (I) / UZNEXT / 3.6E6   *****
      !   50 END DO
      DO iel=1,total_no_elements
         !precip_m_per_s(iel) = (GETSPACETIME1(iel, uznow, uznext) + PTOT(NRAINC(iel)) / UZNEXT) / 3.6E6
         precip_m_per_s(iel) = PTOT(NRAINC(iel)) / UZNEXT / 3.6E6
      ENDDO
      ! read in breakpoint PE for this timestep (if required)
      IFLAG = 2
      CALL METIN (IFLAG)
!RETURN
!8025 WRITE(PPPRI, 9060) UZNEXT, TSOFT, MELAST, METIME
!    WRITE(PPPRI, 9070) (I, PINP (I), PTOT (I), I = 1, NRAIN)
!    CALL ERROR(FFFATAL, 1025, PPPRI, 0, 0, 'INVALID TIMESTEP')
9060  FORMAT(////'UZNEXT = ',G14.6, &
      &          /' TSOFT = ',G14.6, &
      &          /'MELAST = ',G14.6, &
      &          /'METIME = ',G14.6 / &
      &           'PREC.STN.   PINP        PTOT'/)
9070  FORMAT(4X,I4,2G14.6)
   END SUBROUTINE TMSTEP
END MODULE rest
