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
                     VHT1, BMETP, BMETAL, MEASPE, del
USE FRmod,    ONLY : BSOFT
USE ALmod,    ONLY : ALINIT
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
DO 10 I = 0, 100  
   10 IF (FLERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 1000, FLERRC (I)  
DO 20 I = 0, 100  
   20 IF (SYERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 2000, SYERRC (I)  
DO 30 I = 0, 100  
   30 IF (CMERRC (I) .GT.0) WRITE(PPPRI, 1500) I + 3000, CMERRC (I)  
WRITE(PPPRI, 1600)  
 1400 FORMAT(// 'Error message asummary'/)  
 1500 FORMAT('No. of occurences of error number ',I4,': ',I6)  

 1600 FORMAT(/ 'End of error message asummary')  
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
  9900 FORMAT ('Normal completion of SHETRAN run: ',F10.2, ' hours, ', &
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
  200    END DO  

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

  300    END DO  
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
  305       END DO  
  310    END DO  

   asum = asum + asumQ / cellarea (IEL)  
!        * convert from rate to depth


   DEPTHI = asum * DTUZ  
!        Update the cumulative water balance error as a depth
!        ----------------------------------------------------

   WBERR (IEL) = WBERR (IEL) + DELSTO - DEPTHI  



  400 END DO  
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
!  CALCULATIONS.  IT IS ASasumED THAT A MET DATA PREPROGRAM
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
!     OGICAL STATIONS ARE THE SAME, THE STATIONS ARE ASasumED TO HAVE
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
! Input arguments

INTEGER :: IFLAG  
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
! sb 300407 convert breakpoint data to regularly spaced data
    5    READ (PRD, *, END = 281) (PINP (I), I = 1, NRAIN)

   goto 282  
  281    if (firstnoprd) then  
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
   do i = 1, nrain  
   pinp (i) = zero

   enddo  
  282    do i = 1, nrain  
   pinp (i) = pinp (i) / dtmet2  
   enddo  
   MELAST = METIME  



   METIME = METIME+dtmet2  
! check for invalid data
!        IF (METIME.NE.0.0 .AND. MELAST.GT.METIME) THEN
!           WRITE(PRI,2) I1,I2,I3,I4,I5,MELAST,METIME
! 2       FORMAT(////'INPUT PREC. TIME = ',5I5 /
!     -               'MELAST = ',G12.6 /
!     -               'METIME = ',G12.6)
!          CALL ERROR(FATAL,1026,PRI,0,0,
!     -      'INVALID BREAKPOINT PRECIPITATION DATA')
!        ENDIF
!
   IF (BHOTRD.AND.METIME.LT.BHOTTI) GOTO 5  
!
ELSE  
! POT. EVAPOTRANSPIRATION
! first check for hotstarted run
    7    IF (BHOTRD.AND.EPTIME.LT.BHOTTI) THEN  
!          READ (EPD,*,END=280) I1, I2, I3, I4, I5, (PEIN(I),I=1,NM)
!          EPLAST = EPTIME
!          EPTIME = HOUR(I1,I2,I3,I4,I5) - TIH
! sb 300407 convert breakpoint data to regularly spaced data
      READ (EPD, *, END = 283) (PEIN (I), I = 1, NM)  

      goto 284  
  283       if (firstnoepd1) then  
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
      do i = 1, nm  
      pein (i) = zero  

      enddo  
      
 284    if (ista) then
         READ (TAH, *, END = 383) (tahigh (I), I = 1, NM)  
         goto 384
         endif

  383    do i=1,nm
           tahigh(i) = 10.0
         enddo
       
  384    if (ista) then
         READ (TAL, *, END = 483) (talow (I), I = 1, NM)  
         goto 484
         endif
   
  483     do i=1,nm
           talow(i) = 10.0
         enddo

  
  484 do i = 1, nm  
      pein (i) = pein (i) / dtmet3  
      enddo  
      EPLAST = EPTIME  

      EPTIME = EPTIME+dtmet3  
      GOTO 7  


   ENDIF  
! calculate average PE value over computational timestep
   TEND = MIN (UZNOW + UZNEXT, EPTIME)  
   DO 10 I = 1, NM  
      PETOT (I) = (TEND-UZNOW) * PEIN (I)  

   10    END DO  
   12    IF (EPTIME.LT.UZNOW + UZNEXT) THEN  
!          READ (EPD,*,END=280) I1, I2, I3, I4, I5, (PEIN(I),I=1,NM)
!          EPLAST = EPTIME
!          EPTIME = HOUR(I1,I2,I3,I4,I5) - TIH
! sb 300407 convert breakpoint data to regularly spaced data
      READ (EPD, *, END = 285) (PEIN (I), I = 1, NM)  

      goto 286  
  285       if (firstnoepd2) then  
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
      do i = 1, nm  
      pein (i) = zero  

      enddo 
      
  286    if (ista) then
         READ (TAH, *, END = 385) (tahigh (I), I = 1, NM)  
         goto 386
         endif

  385    do i=1,nm
           tahigh(i) = 10.0
         enddo
       
  386    if (ista) then
         READ (TAL, *, END = 485) (talow (I), I = 1, NM)  
         goto 486
         endif
   
  485    do i=1,nm
           talow(i) = 10.0
         enddo
  
  486   do i = 1, nm  
       pein (i) = pein (i) / dtmet3  
       enddo  
      
      EPLAST = EPTIME  
      EPTIME = EPTIME+dtmet3  
! check for invalid data
!          IF (EPTIME.NE.0.0 .AND. EPLAST.GT.EPTIME) THEN
!            WRITE(PRI,3) I1,I2,I3,I4,I5,EPLAST,EPTIME
! 3          FORMAT(////'INPUT PE. TIME = ',5I5 /
!     -                 'EPLAST = ',G12.6 /
!     -                 'EPTIME = ',G12.6)
!            CALL ERROR(FATAL,1026,PRI,0,0,
!     -      'INVALID BREAKPOINT POT. EVAPOTRANSPIRATION DATA')
!        ENDIF
      TEND = MIN (UZNOW + UZNEXT, EPTIME)  
      DO 14 I = 1, NM  
         PETOT (I) = PETOT (I) + (TEND-EPLAST) * PEIN (I)  

   14       END DO  
      GOTO 12  

   ENDIF  
   DO 16 I = 1, NM  
      OBSPE (I) = PETOT (I) / UZNEXT / 3600.  
! for simplicity the temperature used is the value at the end of the timestep
      ta (I) = (tahigh (I) +  talow (I) )/2.0
!      ta (I) = tahigh (I)


   16    END DO  
ENDIF  
!
! PRINT OUT INPUT DATA
!
IF (BMETP) THEN  
   WRITE(PPPRI, 30) METIME  
   30 FORMAT   (//1X, 'MET DATA -  TIME :',F8.2 / &
&   ' STATION           RAINFALL      POT. EVAP.(MM/HR)')
   DO 35 I = 1, NM  
      WRITE(PPPRI, 32) I, PINP (I), PEIN (I)  
   32 FORMAT    (4X,I2,9X,F10.3,9X,F10.3)  
   35    END DO  
ENDIF  
!
GOTO 190  
!
! READ ALL MET. DATA IN FIXED TIME INTERVAL (USUALLY HOURLY) FORMAT
!------------------------------------------------------------------
!
!^^^^^^^^^              GP 29/9/92
   40 IF (IFLAG.EQ.2) RETURN  
!^^^^^^^^^
IF (NRAIN.NE.NM) GOTO 100  
!
!-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE EQUAL
!
IF (BMETP) WRITE(PPPRI, 50)  
   50 FORMAT (//1X, 'MET DATA - SITE    TIME      RAINFALL    NET RADN', &
&       4X, &
& &
&'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
!
!-----LOOP ON NUMBER OF MET SITES
!
   55 MELAST = METIME  

METIME = METIME+DTMET  
DO 90 I = 1, NM  
   READ (MED, 60, END = 287) ISITE, NN, PINP (I), RN (I), U (I), &
    PA (I), TA (I), DEL (I), VPD (I), IDATA

   goto 288  
  287    if (firstnomet1) then  
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
  288    IF (BMETP) WRITE(PPPRI, 70) ISITE, METIME, PINP (I), RN (I), &
    U (I), TA (I), DEL (I), VPD (I)
   60 FORMAT   (2I6, 4G12.6, /, 12X, 3G12.6, I12)  
   70 FORMAT   ('0', 8X, I6, F8.2, 5X, 2(3F12.6,'  NOT_USED  '))  
   IF (MEASPE (I) .EQ.0) GOTO 90  
!
! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
!
   READ (MED, 80, END = 289) OBSPE (I)  
   80 FORMAT   (12X, G12.6)  

   goto 290  
  289    if (firstnomet2) then  
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
  290    OBSPE (I) = OBSPE (I) / 3600.  
   90 END DO  
!
! READ TO START SIMULATION TIME, IF HOTSTART
!
IF (BHOTRD.AND.METIME.LT.BHOTTI) GOTO 55  
!
GOTO 190  
!
!-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE UNEQUAL
!
  100 IF (BMETP) WRITE(PPPRI, 110)  
  110 FORMAT (//1X, 'MET DATA - SITE    TIME      NET RADN', 4X, &
& &
&'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
!
!-----LOOP ON NUMBER OF MET SITES
!
  115 MELAST = METIME  
METIME = METIME+DTMET  
DO 140 I = 1, NM  
   READ (MED, 120, END = 291) ISITE, NN, RN (I), U (I), PA (I), &
    TA (I), DEL (I), VPD (I), IDATA

   goto 292  
  291    if (firstnomet3) then  
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
  292    IF (BMETP) WRITE(PPPRI, 130) ISITE, METIME, RN (I), U (I), &
    TA (I), DEL (I), VPD (I)
  120 FORMAT   (2I6, 12X, 3G12.6, /, 12X, 3G12.6, I12)  
  130 FORMAT   ('0', 8X, I6, F8.2, 5X, 2(2F12.6,'  NOT_USED  ':F12.6))  
   IF (MEASPE (I) .EQ.0) GOTO 140  
!
! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
!
   READ (MED, 80, END = 293) OBSPE (I)  

   goto 294  
  293    if (firstnomet4) then  
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
  294    OBSPE (I) = OBSPE (I) / 3600.  
  140 END DO  
IF (BMETP) WRITE(PPPRI, 150)  

  150 FORMAT (//1X, 'RAIN DATA - SITE    TIME      RAINFALL         IDATA')
!
!-----LOOP ON NUMBER OF RAIN SITES
!
DO 180 I = 1, NRAIN  
   READ (MED, 160, END = 295) ISITE, NN, PINP (I), IDATA  

   goto 296  
  295    if (firstnomet5) then  
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
  296    IF (BMETP) WRITE(PPPRI, 170) ISITE, METIME, PINP (I)  
  160 FORMAT   (2I6, G12.6, 24X, I12)  
  170 FORMAT   ('0', 9X, I6, F8.2, 5X, F12.6, '  NOT_USED  ')  
  180 END DO  
!
! READ TO SIMULATION START TIME, IF HOTSTART
!
IF (BHOTRD.AND.METIME.LT.BHOTTI) GOTO 115  
!
!--------------------------------------------
!     CHECK TIME-VARYING MODEL PARAMETERS
!--------------------------------------------
!
  190 TCURR = TIMEUZ  
DO 270 K = 1, NV  
   IF (MODECS (K) .NE.0) CALL TERPO1 (CSTCAP, TCURR, RELCST, &
    TIMCST, NCTCST, CSTCA1, NVEE, K)
   IF (MODEPL (K) .NE.0) CALL TERPO1 (PLAI, TCURR, RELPLA, TIMPLA, &
    NCTPLA, PLAI1, NVEE, K)
   IF (MODECL (K) .NE.0) CALL TERPO1 (CLAI, TCURR, RELCLA, TIMCLA, &
    NCTCLA, CLAI1, NVEE, K)
   IF (MODEVH (K) .NE.0) CALL TERPO1 (VHT, TCURR, RELVHT, TIMVHT, &
    NCTVHT, VHT1, NVEE, K)
  270 END DO  
!
RETURN  
STOP  
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
! Locals, etc
!INTRINSIC MIN  
INTEGER :: I, IEL, IFLAG  
DOUBLEPRECISION TEND, TSNOW, TSOFT, UZTEST, PTOT(nrain)
LOGICAL :: exitt, SMFLAG, iscycle, jumpto45, first
!----------------------------------------------------------------------*
! ----------------------------------------------------------------------
!  1.  COMPUTE EXPECTED TiMeSTEP
! ----------------------------------------------------------------------
! CALCULATE REDUCED TIMESTEP FOR SOFTSTART
TSOFT = TMAX  

jumpto45 = .FALSE.
IF (BSOFT.AND.NSTEP.LE.102) TSOFT = TMAX * 0.05d0 * 1.03d0**NSTEP  
! CALCULATE REDUCED TIMESTEP FOR SNOWMELT
TSNOW = TMAX  
IF(BEXSM) THEN  
    SMFLAG = .FALSE.  
    DO 5 I = 1, NM  
        IF (GTZERO(TA(I))) SMFLAG = .TRUE.  
    5 ENDDO  
    IF(SMFLAG) THEN
        iscycle = .FALSE.
        DO 7 IEL = total_no_links + 1, total_no_elements
            IF(iscycle) CYCLE  
            IF (GTZERO(SD(IEL))) THEN  
                TSNOW = 0.5  
                iscycle = .TRUE. ! GOTO 8  
            !                    vvvvvv
            ENDIF  
        7 ENDDO  
        ! 8 CONTINUE  
    ENDIF  
ENDIF  
! SET TIMESTEP LENGTH
UZNEXT = MIN (UZNEXT * (1.0 + PALFA), TSOFT, TSNOW)  
! ----------------------------------------------------------------------
!  2.  READ METEOROLOGICAL DATA AND REDUCE TMSTEP IF NECESSARY
! ----------------------------------------------------------------------
! set period of validity of current data
exitt = .FALSE.
first = .TRUE.
DO WHILE((first .OR. exitt) .AND. .NOT.jumpto45)
    first = .FALSE.  
    9 TEND = MIN (UZNOW + UZNEXT, METIME)
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
   30 ENDDO  
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
9060 FORMAT(////'UZNEXT = ',G12.6, &
        &          /' TSOFT = ',G12.6, &
        &          /'MELAST = ',G12.6, &
        &          /'METIME = ',G12.6 / &
        &           'PREC.STN.   PINP        PTOT'/)
 9070 FORMAT(4X,I4,2G12.6)  
END SUBROUTINE TMSTEP
END MODULE rest