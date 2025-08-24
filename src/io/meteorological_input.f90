MODULE meteorological_input
! Extracted from rest.f90 - Meteorological data input functionality
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
! Refactored: 2025-08-22 - Split from rest module for better organization

   USE SGLOBAL
   USE AL_C,    ONLY : CLAI, NV, PLAI, UZNEXT
   USE AL_D,    ONLY : DTMET2, BHOTRD, BHOTTI, EPD, NM, PRD, NRAIN, DTMET3, &
      PE, DTMET, MED, RN, OBSPE, U, TA, VPD, TIMEUZ, VHT, &
      tah, tal, ista
   USE ETmod,   ONLY : MODECS, CSTCAP, RELCST, TIMCST, NCTCST, CSTCA1, MODEPL, RELPLA, TIMPLA, NCTPLA, &
      PLAI1, MODECL, RELCLA, TIMCLA, NCTCLA, CLAI1, MODEVH, RELVHT, TIMVHT, NCTVHT, &
      VHT1, BMETP, BMETAL, MEASPE, del
   USE UTILSMOD, ONLY : TERPO1

   IMPLICIT NONE

   DOUBLEPRECISION :: pinp(nvee+10)=zero, METIME=zero, MELAST=zero, EPTIME=zero

   PRIVATE
   PUBLIC :: METIN, METIME, MELAST, EPTIME, pinp

CONTAINS

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
! Refactored: 2025-08-22 - Split from rest module for better organization
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
      INTEGER :: I, IDATA, ISITE, K, NN, iost
      DOUBLEPRECISION EPLAST, TCURR, TEND
      DOUBLEPRECISION PA (NVEE), PEIN (NVEE), PETOT (NVEE), per(nrain),tahight(nvee),talowt(nvee),tahigh(nvee),talow(nvee)
      logical :: firstnoprd, firstnoepd1, firstnoepd2
      logical :: firstnomet1, firstnomet2, firstnomet3
      logical :: firstnomet4, firstnomet5
      logical :: prd_eof
      data firstnoprd / .true. /
      data firstnoepd1 / .true. /
      data firstnoepd2 / .true. /
      data firstnomet1 / .true. /
      data firstnomet2 / .true. /
      data firstnomet3 / .true. /
      data firstnomet4 / .true. /
      data firstnomet5 / .true. /
      prd_eof = .false.
!----------------------------------------------------------------------*


      IF (BMETAL) THEN
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
            read (PRD, *, IOSTAT=iost) (PINP (I), I = 1, NRAIN)
            if (iost < 0) then
               prd_eof = .true.
            end if

            if (.not. prd_eof) then
               do i = 1, nrain
                  pinp (i) = pinp (i) / dtmet2
               enddo
               MELAST = METIME
               METIME = METIME+dtmet2
            end if
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
            DO WHILE (BHOTRD.AND.METIME.LT.BHOTTI .and. .not. prd_eof)
               read (PRD, *, IOSTAT=iost) (PINP (I), I = 1, NRAIN)
               if (iost < 0) then
                  prd_eof = .true.
                  cycle
               end if
               do i = 1, nrain
                  pinp (i) = pinp (i) / dtmet2
               enddo
               MELAST = METIME
               METIME = METIME+dtmet2
            END DO

            if (prd_eof) then
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
               do i = 1, nrain
                  pinp (i) = zero
               enddo
            end if
!
         ELSE
! POT. EVAPOTRANSPIRATION
! first check for hotstarted run
            DO WHILE(BHOTRD.AND.EPTIME.LT.BHOTTI)
!          read (EPD,*,END=280) I1, I2, I3, I4, I5, (PEIN(I),I=1,NM)
!          EPLAST = EPTIME
!          EPTIME = HOUR(I1,I2,I3,I4,I5) - TIH
! sb 300407 convert breakpoint data to regularly spaced data
               read (EPD, *, IOSTAT=iost) (PEIN (I), I = 1, NM)
               if (iost < 0) then
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
                  do i = 1, nm
                     pein (i) = zero
                  enddo
               end if

               if (ista) then
                  read (TAH, *, IOSTAT=iost) (tahigh (I), I = 1, NM)
                  if (iost < 0) then
                     do i=1,nm
                        tahigh(i) = 10.0
                     enddo
                  end if
               else
                  do i=1,nm
                     tahigh(i) = 10.0
                  enddo
               endif

               if (ista) then
                  read (TAL, *, IOSTAT=iost) (talow (I), I = 1, NM)
                  if (iost < 0) then
                     do i=1,nm
                        talow(i) = 10.0
                     enddo
                  end if
               else
                  do i=1,nm
                     talow(i) = 10.0
                  enddo
               endif

               do i = 1, nm
                  pein (i) = pein (i) / dtmet3
               enddo
               EPLAST = EPTIME

               EPTIME = EPTIME+dtmet3
            END DO


! calculate average PE value over computational timestep
            TEND = MIN (UZNOW + UZNEXT, EPTIME)
            DO I = 1, NM
               PETOT (I) = (TEND-UZNOW) * PEIN (I)

            END DO
            DO WHILE (EPTIME.LT.UZNOW + UZNEXT)
!          read (EPD,*,END=280) I1, I2, I3, I4, I5, (PEIN(I),I=1,NM)
!          EPLAST = EPTIME
!          EPTIME = HOUR(I1,I2,I3,I4,I5) - TIH
! sb 300407 convert breakpoint data to regularly spaced data
               read (EPD, *, IOSTAT=iost) (PEIN (I), I = 1, NM)
               if (iost < 0) then
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
                  do i = 1, nm
                     pein (i) = zero
                  enddo
               end if

               if (ista) then
                  read (TAH, *, IOSTAT=iost) (tahigh (I), I = 1, NM)
                  if (iost < 0) then
                     do i=1,nm
                        tahigh(i) = 10.0
                     enddo
                  end if
               else
                  do i=1,nm
                     tahigh(i) = 10.0
                  enddo
               endif

               if (ista) then
                  read (TAL, *, IOSTAT=iost) (talow (I), I = 1, NM)
                  if (iost < 0) then
                     do i=1,nm
                        talow(i) = 10.0
                     enddo
                  end if
               else
                  do i=1,nm
                     talow(i) = 10.0
                  enddo
               endif

               do i = 1, nm
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
               DO I = 1, NM
                  PETOT (I) = PETOT (I) + (TEND-EPLAST) * PEIN (I)

               END DO
            END DO
            DO I = 1, NM
               OBSPE (I) = PETOT (I) / UZNEXT / 3600.
! for simplicity the temperature used is the value at the end of the timestep
               ta (I) = (tahigh (I) +  talow (I) )/2.0
!      ta (I) = tahigh (I)


            END DO
         ENDIF
!
! PRINT OUT INPUT DATA
!
         IF (BMETP) THEN
            WRITE(PPPRI, 30) METIME
30          FORMAT   (//1X, 'MET DATA -  TIME :',F8.2 / &
            &   ' STATION           RAINFALL      POT. EVAP.(MM/HR)')
            DO I = 1, NM
               WRITE(PPPRI, 32) I, PINP (I), PEIN (I)
32             FORMAT    (4X,I2,9X,F10.3,9X,F10.3)
            END DO
         ENDIF
!
      ELSE
!
! READ ALL MET. DATA IN FIXED TIME INTERVAL (USUALLY HOURLY) FORMAT
!------------------------------------------------------------------
!
!^^^^^^^^^              GP 29/9/92
         IF (IFLAG.EQ.2) RETURN
!^^^^^^^^^
         IF (NRAIN.NE.NM) THEN
!
!-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE UNEQUAL
!
            IF (BMETP) WRITE(PPPRI, 110)
110         FORMAT (//1X, 'MET DATA - SITE    TIME      NET RADN', 4X, &
            & &
            &'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
!
!-----LOOP ON NUMBER OF MET SITES
!
            DO
               MELAST = METIME
               METIME = METIME+DTMET
               DO I = 1, NM
                  read (MED, 120, IOSTAT=iost) ISITE, NN, RN (I), U (I), PA (I), &
                     TA (I), DEL (I), VPD (I), IDATA
                  if (iost < 0) then
                     if (firstnomet3) then
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
                  end if

                  IF (BMETP) WRITE(PPPRI, 130) ISITE, METIME, RN (I), U (I), &
                     TA (I), DEL (I), VPD (I)
120               FORMAT   (2I6, 12X, 3G12.6, /, 12X, 3G12.6, I12)
130               FORMAT   ('0', 8X, I6, F8.2, 5X, 2(2F12.6,'  NOT_USED  ':F12.6))
                  IF (MEASPE (I) .NE.0) THEN
!
! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
!
                     read (MED, 80, IOSTAT=iost) OBSPE (I)
                     if (iost < 0) then
                        if (firstnomet4) then
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
                     end if
!
! CONVERT TO MM/S
!
                     OBSPE (I) = OBSPE (I) / 3600.
                  ENDIF
               END DO
               IF (BMETP) WRITE(PPPRI, 150)

150            FORMAT (//1X, 'RAIN DATA - SITE    TIME      RAINFALL         IDATA')
!
!-----LOOP ON NUMBER OF RAIN SITES
!
               DO I = 1, NRAIN
                  read (MED, 160, IOSTAT=iost) ISITE, NN, PINP (I), IDATA
                  if (iost < 0) then
                     if (firstnomet5) then
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
                  end if

                  IF (BMETP) WRITE(PPPRI, 170) ISITE, METIME, PINP (I)
160               FORMAT   (2I6, G12.6, 24X, I12)
170               FORMAT   ('0', 9X, I6, F8.2, 5X, F12.6, '  NOT_USED  ')
               END DO
!
! READ TO SIMULATION START TIME, IF HOTSTART
!
               IF (.NOT. (BHOTRD.AND.METIME.LT.BHOTTI)) EXIT
            END DO
         ELSE
!
!-----NUMBERS OF RAINFALL AND METEOROLOGICAL STATIONS ARE EQUAL
!
            IF (BMETP) WRITE(PPPRI, 50)
50          FORMAT (//1X, 'MET DATA - SITE    TIME      RAINFALL    NET RADN', &
            &       4X, &
            & &
            &'WIND SPEED  ATMOS PRES   AIR TEMP       DEL        VPD         IDATA')
!
!-----LOOP ON NUMBER OF MET SITES
!
            DO
               MELAST = METIME

               METIME = METIME+DTMET
               DO I = 1, NM
                  read (MED, 60, IOSTAT=iost) ISITE, NN, PINP (I), RN (I), U (I), &
                     PA (I), TA (I), DEL (I), VPD (I), IDATA
                  if (iost < 0) then
                     if (firstnomet1) then
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
                  end if

                  IF (BMETP) WRITE(PPPRI, 70) ISITE, METIME, PINP (I), RN (I), &
                     U (I), TA (I), DEL (I), VPD (I)
60                FORMAT   (2I6, 4G12.6, /, 12X, 3G12.6, I12)
70                FORMAT   ('0', 8X, I6, F8.2, 5X, 2(3F12.6,'  NOT_USED  '))
80                FORMAT   (12X, G12.6)
                  IF (MEASPE (I) .NE.0) THEN
!
! READ MEASURED POTENTIAL EVAPORATION IN MM/HR
!
                     read (MED, 80, IOSTAT=iost) OBSPE (I)
                     if (iost < 0) then
                        if (firstnomet2) then
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
                     end if
!
! CONVERT TO MM/S
!
                     OBSPE (I) = OBSPE (I) / 3600.
                  ENDIF
               END DO
!
! READ TO SIMULATION START TIME, IF HOTSTART
!
               IF (.NOT. (BHOTRD.AND.METIME.LT.BHOTTI)) EXIT
            END DO
         ENDIF
      ENDIF
!
!--------------------------------------------
!     CHECK TIME-VARYING MODEL PARAMETERS
!--------------------------------------------
!
      TCURR = TIMEUZ
      DO K = 1, NV
         IF (MODECS (K) .NE.0) CALL TERPO1 (CSTCAP, TCURR, RELCST, &
            TIMCST, NCTCST, CSTCA1, NVEE, K)
         IF (MODEPL (K) .NE.0) CALL TERPO1 (PLAI, TCURR, RELPLA, TIMPLA, &
            NCTPLA, PLAI1, NVEE, K)
         IF (MODECL (K) .NE.0) CALL TERPO1 (CLAI, TCURR, RELCLA, TIMCLA, &
            NCTCLA, CLAI1, NVEE, K)
         IF (MODEVH (K) .NE.0) CALL TERPO1 (VHT, TCURR, RELVHT, TIMVHT, &
            NCTVHT, VHT1, NVEE, K)
      END DO
!
      RETURN
      STOP
   END SUBROUTINE METIN

END MODULE meteorological_input
