MODULE timestep_control
! Extracted from rest.f90 - Timestep computation and meteorological data reading
! JE  12/08   4.3.5F90  Created, as part of conversion to FORTRAN90
! Refactored: 2025-08-22 - Split from rest module for better organization

   USE SGLOBAL
   USE AL_C,    ONLY : NLYRBT, NV, PLAI, PNETTO, QVSBF, QVSWEL, QBKF, QOC, QVSH, UZNEXT, VSTHE, WBERR
   USE AL_D,    ONLY : nstep, DTMET2, BHOTRD, BHOTTI, EPD, NM, PRD, NRAIN, DTMET3, &
      PE, DTMET, MED, RN, OBSPE, U, TA, VPD, TMAX, VHT, TIMEUZ, SD, PALFA, BEXSM, PMAX, precip_m_per_s, NRAINC
   USE FRmod,   ONLY : BSOFT
   USE meteorological_input, ONLY : METIN, METIME, MELAST, EPTIME, pinp

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: TMSTEP

CONTAINS

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
! Refactored: 2025-08-22 - Split from rest module for better organization
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

!**SB 07072020 reduce timestep if there are errors 1024,1030,1060
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
9060  FORMAT(////'UZNEXT = ',G12.6, &
      &          /' TSOFT = ',G12.6, &
      &          /'MELAST = ',G12.6, &
      &          /'METIME = ',G12.6 / &
      &           'PREC.STN.   PINP        PTOT'/)
9070  FORMAT(4X,I4,2G12.6)
   END SUBROUTINE TMSTEP

END MODULE timestep_control
