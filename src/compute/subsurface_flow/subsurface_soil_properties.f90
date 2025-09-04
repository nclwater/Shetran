MODULE subsurface_soil_properties
!----------------------------------------------------------------------*
! Soil hydraulic property calculations for van Genuchten models
! Contains VSFUNC, VSSOIL, VSSPR - soil property calculators
!----------------------------------------------------------------------*
! Extracted from VSmod.f90 as part of refactoring
! Date: 2025-09-04
! Source: VSmod.f90.sav (lines 1789-1959, 4010-4317, 4321-4363)
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE subsurface_variables
   USE AL_C, ONLY : NS, VSPOR
   USE UTILSMOD, ONLY : FINPUT, HINPUT
   IMPLICIT NONE

   PRIVATE
   ! Public soil property routines
   PUBLIC :: VSFUNC, VSSOIL, VSSPR

CONTAINS

!SSSSSS SUBROUTINE VSFUNC
   SUBROUTINE VSFUNC (NVSSOL_ARG, NSOLEE_ARG, VSPPSI_ARG, VSPTHE_ARG, VSPKR_ARG, &
      VSPETA_ARG, VSPDKR_ARG, VSPDET_ARG, IEL, ICBOT, ICTOP, ICSOIL, CPSI, ICSTOR, &
      CTHETA, CETA, CKR, CDETA, CDKR)
!
!----------------------------------------------------------------------*
! Calculates moisture content, storage coefficient, and relative
! hydraulic conductivity for a column, given soil water potentials
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSFUNC/4.1
! Modifications:
!  GP  18.8.94  written
! RAH  961220  4.1  No long/leading comments.  Declare externals.
!                   Explicit sizes where possible.  ICSTOR is in+out.
!                   No redundant execution or lower-case code.
!      970121       Use arguments, not COMMON.  Allow end-point cases.
!                   Remove redundant arguments and commented code.
!      970122       Amend entry conditions.  Use branch for ERROR call.
!JE  JAN 2009       Loop restructure for AD
!----------------------------------------------------------------------*
! Returns:
!         moisture content (CTHETA),
!         storage co-efficient (CETA),
!         relative hydraulic conductivity (CKR),
!         derivative of storage coefficient(CDETA), and
!         derivative of relative conductivity (CDKR)
! for all cells in a column, given pressure potential (CPSI).
!
! Based on subroutine HUNT of 'Numerical Recipes in FORTRAN: The Art of
!   Scientific Computing (2nd Ed.)', Press et al. (1992), p112
!----------------------------------------------------------------------*
! Entry conditions:
!     PRI is connected for formatted output
! 1 < NVSSOL <= NSOLEE
!     VSPPSI is monotonic strictly decreasing
!      ICBOT <= ICTOP
! 0 < ICSOIL(ICBOT:ICTOP) <= NS (size of 2nd dimension of VSPTHE, etc)
!----------------------------------------------------------------------*
!
! Input arguments
      INTEGER :: NVSSOL_ARG, NSOLEE_ARG
      DOUBLEPRECISION VSPPSI_ARG (NVSSOL_ARG), VSPTHE_ARG (NSOLEE_ARG, * )
      DOUBLEPRECISION VSPKR_ARG (NSOLEE_ARG, * ), VSPETA_ARG (NSOLEE_ARG, * )
      DOUBLEPRECISION VSPDKR_ARG (NSOLEE_ARG, * ), VSPDET_ARG (NSOLEE_ARG, * )
      INTEGER :: IEL, ICBOT, ICTOP, ICSOIL (ICBOT:ICTOP)
      DOUBLEPRECISION CPSI (ICBOT:ICTOP)
!
! In+out arguments
      INTEGER :: ICSTOR (ICBOT:ICTOP)
!
! Output arguments
      DOUBLEPRECISION CTHETA (ICBOT:ICTOP)
      DOUBLEPRECISION CETA (ICBOT:ICTOP), CKR (ICBOT:ICTOP)
      DOUBLEPRECISION CDETA (ICBOT:ICTOP), CDKR (ICBOT:ICTOP)
!
! Locals, etc
!INTRINSIC MAX, MIN, NINT
      CHARACTER (LEN=5) :: WETDRY (0:1)
      DOUBLEPRECISION P, PDUM, VLO
      INTEGER :: ICL, INC, JHI, JLO, JM, IS, DRY
      LOGICAL :: g8100

      DATA WETDRY / '(wet)', '(dry)' /
!
!----------------------------------------------------------------------*
!
! ----- loop over all cells in column
      G8100=.FALSE.
      OUT100 : DO ICL = ICBOT, ICTOP
         IF(g8100) CYCLE
         P = CPSI (ICL)
         JLO = ICSTOR (ICL)



         IS = ICSOIL (ICL)
! --- find location in table of current psi value
! test for initial guess
         IF (JLO.LE.0.OR.JLO.GT.NVSSOL_ARG) THEN
            JLO = 0
            JHI = NVSSOL_ARG + 1
         ELSE
! set initial hunt increment, and hunt up the table
            INC = 1

            IF (P.LE.VSPPSI_ARG (JLO) ) THEN
               ! Hunt up the table
               DO WHILE (.TRUE.)
                  JHI = JLO + INC
                  IF (JHI.GT.NVSSOL_ARG) THEN
                     JHI = NVSSOL_ARG + 1
                     EXIT
                  ELSEIF (P.LE.VSPPSI_ARG (JHI) ) THEN
                     JLO = JHI
                     INC = INC + INC
                  ELSE
                     EXIT
                  ENDIF
               END DO
! hunt down the table
            ELSE
               JHI = JLO
               ! Hunt down the table
               DO WHILE (.TRUE.)
                  JLO = JHI - INC
                  IF (JLO.LT.1) THEN
                     JLO = 0
                     EXIT
                  ELSEIF (P.GT.VSPPSI_ARG (JLO) ) THEN
                     JHI = JLO
                     INC = INC + INC
                  ELSE
                     EXIT
                  ENDIF
               END DO
            ENDIF
         ENDIF
! hunt completed, begin bisection
!       At this point: { VSPPSI(JLO)>=P or JLO=0        } and
!                      { VSPPSI(JHI)< P or JHI=NVSSOL+1 }
         ! Bisection search
         DO WHILE (JHI - JLO.NE.1)
            JM = (JHI + JLO) / 2
            IF (P.LT.VSPPSI_ARG (JM) ) THEN
               JLO = JM
            ELSE
               JHI = JM
            ENDIF
         END DO

         JLO = MAX (1, MIN (JLO, NVSSOL_ARG - 1) )

         JHI = JLO + 1


         ICSTOR (ICL) = JLO
! --- interpolate between values for return variables
         VLO = VSPPSI_ARG (JLO)
         PDUM = (P - VLO) / (VSPPSI_ARG (JHI) - VLO)

         IF (PDUM.LT.ZERO.OR.PDUM.GT.ONE) THEN  !GOTO 8100
            g8100=.TRUE.
            CYCLE out100
         ENDIF
         VLO = VSPTHE_ARG (JLO, IS)
         CTHETA (ICL) = (VSPTHE_ARG (JHI, IS) - VLO) * PDUM + VLO
         CETA (ICL) = VSPETA_ARG (JHI, IS)
         VLO = VSPDKR_ARG (JLO, IS)
         CDKR (ICL) = (VSPDKR_ARG (JHI, IS) - VLO) * PDUM + VLO
         VLO = VSPKR_ARG (JLO, IS)
         CKR (ICL) = (VSPKR_ARG (JHI, IS) - VLO) * PDUM + VLO
         VLO = VSPDET_ARG (JLO, IS)

         CDETA (ICL) = (VSPDET_ARG (JHI, IS) - VLO) * PDUM + VLO
      ENDDO out100
!----------------------------------------------------------------------*
! Exit conditions:
! for each c in ICBOT:ICTOP:
!             0 <  ICSTOR(c) <  NVSSOL
!    VSPPSI(j)  <=   CPSI(c) <= VSPPSI(j+1)
!    VSPTHE(j,s)<= CTHETA(c) <= VSPTHE(j+1,s)
!    VSPETA(j,s)<=   CETA(c) <= VSPETA(j+1,s)
!     VSPKR(j,s)<=    CKR(c) <=  VSPKR(j+1,s)
!    VSPDET(j,s)<=  CDETA(c) <= VSPDET(j+1,s)
!    VSPDKR(j,s)<=   CDKR(c) <= VSPDKR(j+1,s)
! where j=ICSTOR(c) and s=ICSOIL(c)
!----------------------------------------------------------------------*
!RETURN
      IF(g8100) THEN
         DRY = NINT (MAX (ZERO, MIN (PDUM, ONE) ) )  !8100
         CALL ERROR(FFFATAL, 1034 + DRY, PPPRI, IEL, ICL, 'soil property interpolation out of range '//WETDRY (DRY) )
      ENDIF
   END SUBROUTINE VSFUNC

!SSSSSS SUBROUTINE VSSOIL ()
   SUBROUTINE VSSOIL ()
!
!----------------------------------------------------------------------*
! Sets up soil property tables for VSS
!----------------------------------------------------------------------*
! Module:        VSS (0.0)
! Program:       SHETRAN (4.0)
! Callers:       VSIN
! Modifications:
!  GP  20.07.94  written
!----------------------------------------------------------------------*
      INTEGER :: I, IS, NTBPOS (NSEE), NDUM
      DOUBLEPRECISION RVSSOL, PSI, EDUM, EEDUM, DDDUM
      DOUBLEPRECISION DDTSAT, DDTRES, DDA, DDN, DDM, DD1M1, DDTSMR, &
         DDAP, DDAPN, DDAPN1, DDAPM, DDAPM1, DDAPM2, DDTCAP, DDTC, DDTCM, &
         DDTCM1, DDTCM2, DDDTCP

      DOUBLEPRECISION PLOG, PLOGLO, PLOGHI, ADUM, BDUM, HDUM, rkrdum

      PARAMETER (EDUM = 2.718281828D0)


      DATA NTBPOS / NSEE * 1 /
!----------------------------------------------------------------------*
! soil flags:
!       1       van Genuchten
!       2       tabulated theta(psi) and Kr(psi)
!       3       exponential
!       4       tabulated theta(psi), Averjanov Kr (compatible with V3.4
!----------------------------------------------------------------------*
!
! set up size of internal look-up tables
      IF (BFAST) THEN
         NVSSOL = MIN0 (100, NSOLEE)
      ELSE
         NVSSOL = MIN0 (500, NSOLEE)
      ENDIF


      RVSSOL = DBLE (NVSSOL)
! loop over NVSSOL divisions of the soil property tables
! (NB. low values of I correspond to wet soils)
! psi ranges from -(10**-2) to -(10**4)

      DO 500 I = 5, NVSSOL - 1
         PSI = - 10.D0** ( - two + DBLE (6 * (I - 5) ) / RVSSOL)


         VSPPSI (I) = PSI
! set up property data for each soil type, using method ...


         DO 400 IS = 1, NS
! ... 1 (Van Genuchten)

            IF (IVSFLG (IS) .EQ.1) THEN
               DDTSAT = VSPOR (IS)
               DDTRES = VSTRES (IS)
               DDA = VSALPH (IS) * 100.0d0

               DDN = VSVGN (IS)
               DDM = one - (one / DDN)

               DD1M1 = (one / DDM) - one

               DDTSMR = DDTSAT - DDTRES
               DDAP = - DDA * PSI
               DDAPN = DDAP**DDN
               DDAPN1 = DDAP** (DDN - one)
               DDAPM = (one + DDAPN) **DDM
               DDAPM1 = (one + DDAPN) ** (DDM + one)

               DDAPM2 = (one + DDAPN) ** (DDM + two)

               DDDTCP = DDA * DDM * DDN * DDAPN1 / DDAPM1

               VSPTHE (I, IS) = DDTRES + DDTSMR / DDAPM

               VSPDTH (I, IS) = DDTSMR * DDDTCP
               DDTCAP = MAX (1.0d-10, (VSPTHE (I, IS) - DDTRES) &
                  / DDTSMR)
               DDTC = one - (DDTCAP** (one / DDM) )
               DDTCM = DDTC**DDM
               DDTCM1 = DDTC** (DDM - one)

               DDTCM2 = (one - DDTCM) **two


               VSPKR (I, IS) = DSQRT (DDTCAP) * DDTCM2
!            VSPDKR(I,IS) = DSQRT(DDTCAP)*(one-DDTCM)*
!     -        (half*(one-DDTCM)/DDTCAP +
!     -         two*DDTCM1*DDTCAP**DD1M1) * DDDTCP

               DDDUM = (DDA * DDA * DDM * DDN * DDTSMR * DDAPN1 / &
                  DDAPM2) * ( (DDN - one) * (one + DDAPN) + (DDM + &
                  one) * DDN * DDAPN1)
               VSPETA (I, IS) = VSPTHE (I, IS) * VSPSS (IS) / VSPOR (IS) &
                  + VSPDTH (I, IS)
!cc            VSPDET(I,IS) = VSPDTH(I,IS)*VSPSS(IS)/VSPOR(IS) +
!cc     -                     DDDUM


               vspdet (i, is) = zero
! ... 2 (tabulated theta and Kr)


            ELSEIF (IVSFLG (IS) .EQ.2) THEN
!               check for correct location in input table
!               (interpolate between positions NTBPOS(IS) and NTBPOS(IS+
               IF (PSI.LT.TBPSI (NTBPOS (IS) + 1, IS) ) NTBPOS (IS) &
                  = NTBPOS (IS) + 1


               NDUM = NTBPOS (IS)
!               evaluate cubic spline polynomial for theta and Kr
               PLOG = DLOG10 ( - PSI)
               PLOGHI = DLOG10 ( - TBPSI (NDUM + 1, IS) )
               PLOGLO = DLOG10 ( - TBPSI (NDUM, IS) )
               HDUM = PLOGHI - PLOGLO
               ADUM = (PLOGHI - PLOG) / HDUM

               BDUM = (PLOG - PLOGLO) / HDUM
               VSPTHE (I, IS) = ADUM * TBTHE (NDUM, IS) + BDUM * TBTHE ( &
                  NDUM + 1, IS) + ( (ADUM**three - ADUM) * TBTHEC (NDUM, &
                  IS) + (BDUM**three - BDUM) * TBTHEC (NDUM + 1, IS) ) &
                  * (HDUM**two) / 6.0D0

               VSPTHE (I, IS) = VSPOR (IS) * VSPTHE (I, IS)


               VSPKR (I, IS) = ADUM * TBKR (NDUM, IS) + BDUM * TBKR ( &
                  NDUM + 1, IS) + ( (ADUM**three - ADUM) * TBKRC (NDUM, IS) &
                  + (BDUM**three - BDUM) * TBKRC (NDUM + 1, IS) ) * &
                  (HDUM**two) / 6.0D0
! ... 3 (exponential)

            ELSEIF (IVSFLG (IS) .EQ.3) THEN

               EEDUM = EDUM** (VSALPH (IS) * PSI)
               VSPTHE (I, IS) = VSTRES (IS) + (VSPOR (IS) - VSTRES (IS) &
                  ) * EEDUM
               VSPDTH (I, IS) = (VSPOR (IS) - VSTRES (IS) ) * VSALPH ( &
                  IS) * EEDUM
               DDDUM = VSPDTH (I, IS) * VSALPH (IS)
               VSPKR (I, IS) = EEDUM

               VSPDKR (I, IS) = VSALPH (IS) * EEDUM
               VSPETA (I, IS) = VSPTHE (I, IS) * VSPSS (IS) / VSPOR (IS) &
                  + VSPDTH (I, IS)


               VSPDET (I, IS) = VSPDTH (I, IS) * VSPSS (IS) / VSPOR (IS) &
                  + DDDUM
! ... 2/4 (tabulated theta and Kr / tabulated theta and Averjanov Kr)

            ELSEIF (IVSFLG (IS) .EQ.4) THEN

               stop 'UNFINISHED code for soil properties type 4'

            ENDIF

400      END DO


500   END DO
! set up property data for extreme dry conditions

      VSPPSI (NVSSOL) = - 1.0D6
      DO 700 IS = 1, NS
         VSPTHE (NVSSOL, IS) = VSTRES (IS)
         VSPKR (NVSSOL, IS) = zero
         VSPETA (NVSSOL, IS) = zero
         VSPDTH (NVSSOL, IS) = zero
         VSPDKR (NVSSOL, IS) = zero
         VSPDET (NVSSOL, IS) = zero


700   END DO
! set up storage term for tabulated data
      DO 540 I = 5, NVSSOL - 1
         DO 520 IS = 1, NS
            IF (IVSFLG (IS) .EQ.2.OR.IVSFLG (IS) .EQ.4) VSPDTH (I, IS) &
               = (VSPTHE (I + 1, IS) - VSPTHE (I, IS) ) / (VSPPSI (I + 1) &
               - VSPPSI (I) )
            VSPETA (I, IS) = VSPTHE (I, IS) * VSPSS (IS) / VSPOR (IS) &
               + VSPDTH (I, IS)
520      END DO

540   END DO
      DO 560 I = 5, NVSSOL - 1
         DO 550 IS = 1, NS
            IF (IVSFLG (IS) .EQ.2.OR.IVSFLG (IS) .EQ.4) VSPDET (I, IS) &
               = VSPDTH (I, IS) * VSPSS (IS) / VSPOR (IS) + (VSPDTH (I + 1, &
               IS) - VSPDTH (I, IS) ) / (VSPPSI (I + 1) - VSPPSI (I) )
550      END DO


560   END DO
! set up property data for extreme wet conditions
      VSPPSI (4) = zero
      VSPPSI (3) = 2.5d-1
      VSPPSI (2) = 5.0D-1

      VSPPSI (1) = 1.0D6
      DO 600 IS = 1, NS
         VSPKR (4, IS) = one
         VSPKR (3, IS) = one
         VSPKR (2, IS) = one
         VSPKR (1, IS) = one
         VSPETA (4, IS) = vspeta (5, is)
         VSPETA (3, IS) = vspeta (4, is)
         VSPETA (2, IS) = VSPSS (IS)
         VSPETA (1, IS) = VSPSS (IS)
         VSPDTH (4, IS) = vspdth (5, is)
         VSPTHE (4, IS) = vspor (is)
         VSPTHE (3, IS) = vspor (is) + vspeta (4, is) * (vsppsi (3) &
            - vsppsi (4) )
         VSPTHE (2, IS) = vspthe (3, is) + vspeta (3, is) * (vsppsi (2) &
            - vsppsi (3) )
         VSPTHE (1, IS) = vspthe (2, is) + vspss (is) * (vsppsi (1) &
            - vsppsi (2) )
         VSPDTH (3, IS) = zero
         VSPDTH (2, IS) = zero
         VSPDTH (1, IS) = zero
         VSPDKR (4, IS) = vspdkr (5, is)
         VSPDKR (3, IS) = zero
         VSPDKR (2, IS) = zero
         VSPDKR (1, IS) = zero
!        VSPDET(3,IS) = vspdet(4,is)
         VSPDET (4, IS) = zero
         VSPDET (3, IS) = zero
         VSPDET (2, IS) = zero
         VSPDET (1, IS) = zero





600   END DO
! adjust theta for specific storage in the unsaturated zone
!      delpsi=0.0
!      do 610 i=nvssol-1,3,-1
!        delpsi = delpsi+vspthe(i,is)*(vsppsi(i)-vsppsi(i+1))
!        do 605 is=1,ns
!          vspthe(i,is) = vspthe(i,is) *
!     -      (one + vspss(is)*delpsi/vspor(is))
! 605    continue
! 610  continue
! add increment to eta, for stability near water table
!      DO 660 IS=1,NS
!        ETAMAX = 0.0D0
!        DO 620 I=1,NVSSOL
!          ETAMAX = MAX(ETAMAX,VSPETA(I,IS))
! 620    CONTINUE
!        DO 640 I=1,NVSSOL
!          VSPETA(I,IS) = VSPETA(I,IS) +
!     -      0.1d0*ETAMAX*MAX( (1.0D0-DABS(VSPPSI(I))), 0.0D0)
! 640    CONTINUE
! 660  CONTINUE
! DSATG-specific code - adjust relative conductivity curves so that
! Kr approaches unity at saturation (for values of VG-n less than 2,
! the value of Kr drops rapidly and unphysically less than one near satu
      do 680 is = 1, ns
         rkrdum = vspor (is) - vstres (is)
         do 670 i = 5, nvssol
            vspkr (i, is) = ( (vspthe (i, is) - vstres (is) ) / rkrdum) &
               **two
670      end do



680   end do
! write soil property tables to PRI file

      IF (BSOILP) THEN

         WRITE(PPPRI, 905) NS, NVSSOL
         DO 800 IS = 1, NS
            WRITE(PPPRI, 910) IS
            DO 820 I = 1, NVSSOL
               WRITE(PPPRI, 920) I, VSPPSI (I), VSPTHE (I, IS), VSPETA ( &
                  I, IS), VSPKR (I, IS), VSPDTH (I, IS), VSPDET (I, IS), &
                  VSPDKR (I, IS)
820         END DO

800      END DO

      ENDIF

905   FORMAT(/ 'VSS physical soil/lithology property data' / &
      &         '=========================================' / &
      &         I3, ' soils' / &
      &         I3, ' values in soil property tables' )

910   FORMAT(/ &
      & 3X,'  Soil property tables for soil/lithology type: ',I3 / &
      & 3X,'  -------------------------------------------------' // &
      & 3X,'      psi         theta          eta            Kr      ', &
      & ' d(the)/d(psi) d(eta)/d(psi)  d(Kr)/d(psi)' / &
      & 3X,'   (VSPPSI)      (VSPTHE)      (VSPETA)       (VSPKR)   ', &
      & '   (VSPDTH)      (VSPDET)       (VSPDKR)  ' / &
      & 3X,'  ------------  ------------  ------------  ------------', &
      & '  ------------  ------------  ------------' )

920   FORMAT(I3,7(2X,G12.6))
      RETURN
   END SUBROUTINE VSSOIL

!SSSSSS SUBROUTINE VSSPR (CZ, CZSP, CCS, CPSI, CKR, CDKR, CB, CR, CQSP)
   SUBROUTINE VSSPR (CZ, CZSP, CCS, CPSI, CKR, CDKR, CB, CR, CQSP)
!
!----------------------------------------------------------------------*
! Sets up coefficients for column spring discharge
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSSPR/4.1
! Modifications:
!  GP  22.08.94  written
! RAH  970120  4.1  No leading comments.  Use local DHDUM.
!      970127       Use arguments, not INCLUDE.
!----------------------------------------------------------------------*
!
! Input arguments
      DOUBLEPRECISION CZ, CZSP, CCS, CPSI, CKR, CDKR
!
! In+out arguments
      DOUBLEPRECISION CB, CR
!
! Output arguments
      DOUBLEPRECISION CQSP
!
! Locals, etc
      DOUBLEPRECISION DHDUM
!
!----------------------------------------------------------------------*
!

      DHDUM = CPSI + CZ - CZSP

      IF (GEZERO(DHDUM)) THEN

         CQSP = CCS * CKR * DHDUM
         CR = CR + CQSP

         CB = CB - CCS * CDKR

      ELSE

         CQSP = zero

      ENDIF
   END SUBROUTINE VSSPR


END MODULE
