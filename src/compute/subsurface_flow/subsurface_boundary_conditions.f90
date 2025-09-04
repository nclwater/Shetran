MODULE subsurface_boundary_conditions
!----------------------------------------------------------------------*
! Boundary condition implementations and time-varying BC preparation
! Contains VSBC, VSSAI, VSLOWR, VSUPPR, VSWELL, VSPREP
!----------------------------------------------------------------------*
! Extracted from VSmod.f90 as part of refactoring
! Date: 2025-09-04
! Source: VSmod.f90.sav (lines 178-310, 3427-3515, 2324-2380, 4369-4433, 4437-4509, 2519-2663)
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE subsurface_variables
   USE AL_G, ONLY : ICMREF
   USE AL_C, ONLY : BHB, BFB, bexbk, DTUZ, deltaz, dummy, DHF, ESOILA, ERUZ, EEVAP, &
      FHBED, ISORT, jvsacn, JVSDEL, idum, icmbk, LFB, LHB, LINKNS, lgb, &
      NWELBT, NWELTP, NVSSPC, NVSWLI, NTSOIL, nhbed, NVC, NRD, nlyrbt, NVSWLT, NVSSPT, NBFACE, NS, nlyr, &
      PNETTO, QVSSPR, QVSBF, QH, QVSWEL, QBKF, QBKB, QVSV, QVSWLI, QVSH, QBKI, &
      tih, UZNEXT, &
      vsd, VSI, VSPSI, VSTHE, VSPOR, WLD, ZVSPSL, zlyrbt, zvsnod, zbeff, INITIALISE_AL_C, TIH
   USE AL_D, ONLY : TTH
   USE UTILSMOD, ONLY : FINPUT, HINPUT
   IMPLICIT NONE

   PRIVATE
   ! Public boundary condition routines
   PUBLIC :: VSBC, VSSAI, VSLOWR, VSUPPR, VSWELL, VSPREP

CONTAINS

!SSSSSS SUBROUTINE VSBC (BCHELE, FACE, ICBOT, ICTOP, JCBC, ICLYRB, ICLFN, &
   SUBROUTINE VSBC (BCHELE, FACE, ICBOT, ICTOP, JCBC, ICLYRB, ICLFN, &
      ICLFL, ICLHN, ICLHL, CZG, CDELL, CDELZ, CZ, CAIJ, CLF, CLH, CPSI, &
      CKIJ, CDKIJ, CB, CR, CQH, DUM)
!----------------------------------------------------------------------*
! Sets up coefficients for column user-defined boundary conditions
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSBC/4.1
! Modifications:
!  GP  22.08.94  written (v4.0 finished 8.8.95)
! RAH  970120  4.1  No leading comments.  No lower-case code.
!                   Combine IF-blocks.  Use generic intrinsics.
!      970127       Use arguments, not INCLUDE.  Use DUM for TDUM,HDUM.
!      970514       Scrap workspace arg CDQH; set CB & CR directly.
!                   Don't initialize CQH (see VSCOLM).  New local QTOT.
!                   Add arg FACE (=1:4) & 1st dim to arrays CAIJ & CQH.
!      970813       Amend CLF & DUM subscripts: use I not ILYR.
!----------------------------------------------------------------------*
! Entry conditions:
! 1 <= FACE  <= 4
! 1 <= ICBOT <= ICTOP <=   LLEE (size of DUM)
! 0 <= ICLFN          <= NLYREE (size of ICLFL,CLF)
! 0 <= ICLHN          <= NLYREE (size of ICLHL,CLH,DUM)
! 0 <  CDELL
! for each i in 1:ICLFN:
!            1 <= ICLFL(i) < NLYREE (size of ICLYRB)
!        ICBOT <= ICLYRB(y)
!    1 + ICTOP >= ICLYRB(y+1)
! where y=ICLFL(i)
! for each i in 1:ICLHN:
!            1 <= ICLHL(i) < NLYREE (size of ICLYRB)
!        ICBOT <= ICLYRB(y)
!    1 + ICTOP >= ICLYRB(y+1)
! where y=ICLHL(i)
! for each c in ICBOT:ICTOP: 0 < CDELZ(c), CKIJ(c)
!----------------------------------------------------------------------*
! Input arguments
      LOGICAL :: BCHELE
      INTEGER :: FACE, ICBOT, ICTOP, JCBC, ICLYRB ( * )
      INTEGER :: ICLFN, ICLFL ( * ), ICLHN, ICLHL ( * )
      DOUBLEPRECISION CDELZ (ICBOT:ICTOP), CZ (ICBOT:ICTOP), CZG, CDELL
      DOUBLEPRECISION CAIJ (4, ICBOT:ICTOP), CPSI (ICBOT:ICTOP), &
         CLF ( * )

      DOUBLEPRECISION CKIJ (ICBOT:ICTOP), CDKIJ (ICBOT:ICTOP), CLH ( * )
! In+out arguments

      DOUBLEPRECISION CB (ICBOT:ICTOP), CR (ICBOT:ICTOP)
! Output arguments

      DOUBLEPRECISION CQH (4, ICBOT:ICTOP)
! Workspace arguments

      DOUBLEPRECISION DUM ( * )
! Locals, etc
!INTRINSIC MAX
      INTEGER :: ICL, I, ILYR, ICL1, ICL2, IDUM, SGN


      DOUBLEPRECISION ADHOL, AOL, KDUM, Q, QTOT, TICL, TTOT, ZDUM
!----------------------------------------------------------------------*
! flow (type 3)

      IF (JCBC.EQ.3) THEN
         DO 200 I = 1, MAX (1, ICLFN)
            IF (ICLFN.EQ.0) THEN
               ICL1 = ICBOT
               ICL2 = ICTOP
            ELSE
               ILYR = ICLFL (I)
               ICL1 = ICLYRB (ILYR)
               ICL2 = ICLYRB (ILYR + 1) - 1
            ENDIF
            TTOT = zero
            DO 160 ICL = ICL1, ICL2
               TICL = CKIJ (ICL) * CDELZ (ICL)
               DUM (ICL) = TICL
               TTOT = TTOT + TICL
160         END DO
            QTOT = CLF (I)
            DO 180 ICL = ICL1, ICL2
               Q = (DUM (ICL) / TTOT) * QTOT
               CR (ICL) = CR (ICL) - Q
               CQH (FACE, ICL) = Q
180         END DO

200      END DO
! head (type 4)
! NB. If BCHELE=.false., head b.c.'s are depths below ground surface

      ELSEIF (JCBC.EQ.4) THEN
         IF (BCHELE) THEN
            ZDUM = zero
            SGN = + 1
         ELSE
            ZDUM = CZG
            SGN = - 1
         ENDIF
         IDUM = MAX (ICLHN, 1)
         DO 210 I = 1, IDUM
            DUM (I) = ZDUM + SGN * CLH (I)

210      END DO
         DO 260 I = 1, IDUM
            IF (ICLHN.EQ.0) THEN
               ICL1 = ICBOT
               ICL2 = ICTOP
            ELSE
               ILYR = ICLHL (I)
               ICL1 = ICLYRB (ILYR)
               ICL2 = ICLYRB (ILYR + 1) - 1
            ENDIF
            DO 240 ICL = ICL1, ICL2
               AOL = CAIJ (FACE, ICL) / CDELL
               ADHOL = (DUM (I) - CZ (ICL) - CPSI (ICL) ) * AOL
               KDUM = CKIJ (ICL)
               Q = KDUM * ADHOL
               CB (ICL) = CB (ICL) + CDKIJ (ICL) * ADHOL + KDUM * AOL
               CR (ICL) = CR (ICL) - Q
               CQH (FACE, ICL) = Q
240         END DO


260      END DO
! head gradient (type 5)

      ELSEIF (JCBC.EQ.5) THEN

         !STOP 'unfinished code for boundary type 5 - head gradients'
         print*,  'unfinished code for boundary type 5 - head gradients'

      ENDIF
   END SUBROUTINE VSBC

!SSSSSS SUBROUTINE VSSAI (FACE, JCBC, ICBOT, ICTOP, ICBED, CDELL, CZ, &
   SUBROUTINE VSSAI (FACE, JCBC, ICBOT, ICTOP, ICBED, CDELL, CZ, &
      CAIJ, CZS, CPSI, CKIJ, CDKIJ, CB, CR, CQH, depadj, cdelz)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!!!! depadj added, SPA, 03/11/98
!----------------------------------------------------------------------*
! Sets up coefficients for column stream-aquifer interaction
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSSAI/4.1
! Modifications:
!  GP  22.08.94  written (v4.0 finished 15.01.96)
! RAH  970121  4.1  IDUM is INTEGER not DOUBLEPRECISION.
!                   Use AOL,DH,KIJ to reduce number of operations.
!      970203       Use arguments, not INCLUDE.  Add some comments.
!      970211       Remove outputs CQBKB,CQBKF (see VSSIM).
!      970514       Add arg FACE & 1st dim to arrays CAIJ & CQH.
!----------------------------------------------------------------------*
! Entry conditions:
!     1 <= FACE <= 4
! ICBOT <= ICBED+1, ICTOP
!     0 <  CDELL
!----------------------------------------------------------------------*
! Input arguments
      INTEGER :: FACE, JCBC, ICBOT, ICTOP, ICBED
      DOUBLEPRECISION CDELL, CZ (ICBOT:ICTOP), CAIJ (4, ICBOT:ICTOP)
      DOUBLEPRECISION CZS, CPSI (ICBOT:ICTOP)
      DOUBLEPRECISION CKIJ (ICBOT:ICTOP), CDKIJ (ICBOT:ICTOP)
!!!!!! SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      DOUBLEPRECISION depadj
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! In+out arguments

      DOUBLEPRECISION CB (ICBOT:ICTOP), CR (ICBOT:ICTOP)
! Output arguments

      DOUBLEPRECISION CQH (4, ICBOT:ICTOP)
! Locals, etc
      INTEGER :: ICL, IDUM
      DOUBLEPRECISION QDUM, DQDUM, AOL, DH, KIJ
! !!!! SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^



      DOUBLEPRECISION ddum, cdelz (icbot:ictop)
!^^^^^^^^^^^^^^^^^^^^^^^^^^
!----------------------------------------------------------------------*
! set lowest cell in exposed bank face
      IF (JCBC.EQ.9) THEN
!        * in effect stream bed is at base of current land element
         IDUM = ICBOT
      ELSE
!        * stream-aquifer interaction with banks
         IDUM = ICBED+1


      ENDIF
! loop over appropriate cells

      DO 200 ICL = IDUM, ICTOP

         DH = CZS - CZ (ICL) - CPSI (ICL)
! !!!!! change to calculation of AOL for flow out of channel
! limits flows if depth of water in channel is low, or zero
! SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         ddum = 1.0
         if (GTZERO(dh)) ddum = min (one, depadj / cdelz (icl) )

         AOL = (ddum * CAIJ (FACE, ICL) ) / CDELL
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         KIJ = CKIJ (ICL)
! !!!! SPA, 03/11/98.  Change definition of flow derivative
!        DQDUM =   ( CDKIJ(ICL)*DH - KIJ ) * AOL
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         dqdum = - kij * aol
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

         QDUM = KIJ * DH * AOL
         CQH (FACE, ICL) = QDUM
         CB (ICL) = CB (ICL) + DQDUM

         CR (ICL) = CR (ICL) - QDUM

200   END DO
   END SUBROUTINE VSSAI

!SSSSSS SUBROUTINE VSLOWR (JCBC, CA0, CZ, CDELZ, CKZS, CBF, CBH, CPSI, &
   SUBROUTINE VSLOWR (JCBC, CA0, CZ, CDELZ, CKZS, CBF, CBH, CPSI, &
      CKR, CDKR, CB, CR, CQV)
!
!----------------------------------------------------------------------*
! Sets up coefficients for column lower boundary condition
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSLOWR/4.1
! Modifications:
!  GP  22.08.94  written
! RAH  970120  4.1  No leading comments.  No lower-case code.
!                   Combine IF-blocks.  Use local CQVDUM.
!      970131       Use arguments, not INCLUDE.  CDQDUM DBLE, not DOUBLEPRECISION.
!----------------------------------------------------------------------*
! Entry conditions:
! 0 < CDELZ
!----------------------------------------------------------------------*
!
! Input arguments
      INTEGER :: JCBC
      DOUBLEPRECISION CA0, CZ, CDELZ, CKZS, CBF, CBH, CPSI, CKR, CDKR
!
! In+out arguments
      DOUBLEPRECISION CB, CR
!
! Output arguments
      DOUBLEPRECISION CQV
!
! Locals, etc
      DOUBLEPRECISION CDQDUM, CQVDUM, DH, KSODZ
!
!----------------------------------------------------------------------*
!
! column base flow (type 6)
      IF (JCBC.EQ.6) THEN
         CQVDUM = CBF

         CDQDUM = zero
! column base head (type 7)
      ELSEIF (JCBC.EQ.7) THEN
         DH = CBH - CZ - CPSI
         KSODZ = CKZS / (half * CDELZ)
         CQVDUM = KSODZ * CKR * DH

         CDQDUM = KSODZ * (CDKR * DH - CKR)
! no flow (970131: Check column base free drainage (type 8)!)
      ELSE
         CQVDUM = zero

         CDQDUM = zero

      ENDIF
      CQV = CQVDUM
      CB = CB + CA0 * CDQDUM

      CR = CR - CA0 * CQVDUM
   END SUBROUTINE VSLOWR

!SSSSSS SUBROUTINE VSUPPR (CA0, CDELZ, CKZS, DT, CDNET, CPSI, CB, CR, &
   SUBROUTINE VSUPPR (CA0, CDELZ, CKZS, DT, CDNET, CPSI, CB, CR, &
      CQINF)
!----------------------------------------------------------------------*
! Sets up coefficients for column upper boundary condition
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSUPPR/4.2
! Modifications:
!  GP  22.08.94  written (v4.0 finished 20.12.95)
! RAH  970120  4.1  No leading or long comments.  No lower-case code.
!                   Use MAX not MAX.  Rearrange expressions.
!                   Don't INCLUDE AL.G.
!      970127       Use arguments, not COMMON.
!      970514       Replace CDW + (CQP - CEW)*DT with CDNET (see VSSIM).
! RAH  981104  4.2  Rename locals DUM? as QIN,etc.
!----------------------------------------------------------------------*
! Entry conditions:
! 0 < CDELZ, DT
!----------------------------------------------------------------------*
! Input arguments
      DOUBLEPRECISION CA0, CDELZ, CKZS, DT, CDNET, CPSI
! In+out arguments

      DOUBLEPRECISION CB, CR
! Output arguments

      DOUBLEPRECISION CQINF
! Locals, etc
!INTRINSIC MAX


      DOUBLEPRECISION QIN, QOUT, CDQINF, DZO2
!----------------------------------------------------------------------*
! CDNET = total net available depth of surface water after evaporation
! QIN   = infiltration rate which would exhaust CDNET
! QOUT  = exfiltration rate based on transport (-ve for infiltration)
! CQINF = calculated exfiltration rate
!         (+ve upwards, to be consistent with the global array, QH)
!----------------------------------------------------------------------*
      DZO2 = half * CDELZ
      QIN = CDNET / DT
      CDQINF = CKZS / DZO2


      QOUT = CDQINF * (CPSI - (MAX (CDNET, ZERO) + DZO2) )
! infiltration (limited by available water) or evaporation

      IF (QIN.LT. - QOUT) THEN
         CQINF = - QIN


         CDQINF = ZERO
! infiltration (limited by soil properties) or exfiltration

      ELSE

         CQINF = QOUT


      ENDIF
! add into right-hand-side of column tridiagonal system
      CB = CB - CDQINF * CA0

      CR = CR + CQINF * CA0
   END SUBROUTINE VSUPPR

!SSSSSS SUBROUTINE VSWELL (NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL, CA0, &
   SUBROUTINE VSWELL (NSEE, VSK3D, ICWLBT, ICWLTP, ICSOIL, CA0, &
      CDELZ, CQWIN, CPSI, CR, CQWI, RKZDUM)
!----------------------------------------------------------------------*
! Sets up coefficients for column well abstraction
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSWELL/4.1
! Modifications:
!  GP  22.08.94  written (v4.0 finished 28.02.95)
! RAH  970120  4.1  Use generic intrinsics.  Use local QDUM.
!      970127       Use arguments, not INCLUDE.
!      970207       Redefine CQWI: divide by CA0.  Remove output CQW.
!      970514       New args NSEE,ICSOIL,VSK3D in place of LLEE,CKIJS.
!                   Rearrange QDUM expression.
!----------------------------------------------------------------------*
! Entry conditions:
! ICWLBT <= ICWLTP
!      1 <= ICSOIL(ICWLBT:ICWLTP) <= NSEE
!      0 < CA0, CDELZ(ICWLBT:ICWLTP+1), VSK3D(ICSOIL(ICWLBT:ICWLTP),1:2)
!----------------------------------------------------------------------*
! Input arguments
      INTEGER :: NSEE, ICWLBT, ICWLTP, ICSOIL (ICWLBT:ICWLTP)
      DOUBLEPRECISION CDELZ (ICWLBT:ICWLTP + 1), VSK3D (NSEE, 2), &
         CA0

      DOUBLEPRECISION CPSI (ICWLBT:ICWLTP), CQWIN
! In+out arguments

      DOUBLEPRECISION CR (ICWLBT:ICWLTP)
! Output arguments

      DOUBLEPRECISION CQWI (ICWLBT:ICWLTP)
! Workspace arguments

      DOUBLEPRECISION RKZDUM (ICWLBT:ICWLTP)
! Locals, etc
!INTRINSIC MAX, MIN
      INTEGER :: ICL, SOIL




      DOUBLEPRECISION RKZTOT, DZDUM, PDUM, QDUM, RKZ
!----------------------------------------------------------------------*
! The value of CQWIN is the prescribed abstraction rate (m3/s).
! The actual abstraction rate CQWI (m/s) may be less than this if some
! of the aquifer around the well screen becomes unsaturated
! (ie if CPSI(ICL) < DZDUM below).
! calculate product of mean lateral hydraulic conductivity & cell depth
      RKZTOT = ZERO
      DO 50 ICL = ICWLBT, ICWLTP
         SOIL = ICSOIL (ICL)
         RKZ = half * (VSK3D (SOIL, 1) + VSK3D (SOIL, 2) ) * CDELZ ( &
            ICL)
         RKZDUM (ICL) = RKZ
         RKZTOT = RKZ + RKZTOT


50    END DO
! calculate flow into well for each cell, & add into matrix coefficients

      DO 100 ICL = ICWLBT, ICWLTP
         DZDUM = half * (CDELZ (ICL) + CDELZ (ICL + 1) )
         PDUM = MIN (DZDUM, MAX (CPSI (ICL), ZERO) )

         QDUM = CQWIN * (RKZDUM (ICL) / RKZTOT) * (PDUM / DZDUM)
         CQWI (ICL) = QDUM / CA0

         CR (ICL) = QDUM + CR (ICL)


100   END DO
   END SUBROUTINE VSWELL

!SSSSSS SUBROUTINE VSPREP ()
   SUBROUTINE VSPREP ()
!----------------------------------------------------------------------*
! Prepares catchment, and controls reading of time-varying boundary
! conditions
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSPREP/4.1
! Modifications:
!  GP  29.07.94  written (v4.0 finished 3/5/95)
! RAH  961228  4.1  Remove variables IEL,ICL.  No leading comments.
!                   Declare ERROR external.  No lower-case code.
!                   Use SAVE instead of ineffectual COMMON.
!      970213       Reverse subscripts RLFNOW,RLHNOW,RLGNOW (see VSSIM).
!      970522       Initialize saved locals.
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P:            NVSEE
! Input common
!     ...
! Output common
!     VSCOM1.INC:      ...
      INTEGER :: NDATA
      PARAMETER (NDATA = 4 + 3 * NVSEE)
      INTEGER :: I, II, III, NDUM
!DOUBLEPRECISION WLLAST, WLTIME, RWELIN (NVSEE)
!DOUBLEPRECISION RLFLST, RLFTIM, RLFPRV (NVSEE)
!DOUBLEPRECISION RLHLST, RLHTIM, RLHPRV (NVSEE), RLHNXT (NVSEE)
!DOUBLEPRECISION RLGLST, RLGTIM, RLGPRV (NVSEE), RLGNXT (NVSEE)
!DOUBLEPRECISION RBFLST, RBFTIM, RBFPRV (NVSEE)
!DOUBLEPRECISION RBHLST, RBHTIM, RBHPRV (NVSEE), RBHNXT (NVSEE)

!DOUBLEPRECISION RLFDUM (NVSEE), RLHDUM (NVSEE), RLGDUM (NVSEE)
!SAVE WLLAST, WLTIME, RWELIN, RLHLST, RLHTIM, RLHPRV, RLHNXT
!SAVE RLFLST, RLFTIM, RLFPRV, RLGLST, RLGTIM, RLGPRV, RLGNXT
!SAVE RBFLST, RBFTIM, RBFPRV, RBHLST, RBHTIM, RBHPRV, RBHNXT
!DATA WLLAST, WLTIME, RWELIN, RLHLST, RLHTIM, RLHPRV, RLHNXT / &
! NDATA * 0.0D0 /
!DATA RLFLST, RLFTIM, RLFPRV, RLGLST, RLGTIM, RLGPRV, RLGNXT / &
! NDATA * 0.0D0 /
!DATA RBFLST, RBFTIM, RBFPRV, RBHLST, RBHTIM, RBHPRV, RBHNXT / &
! NDATA * 0.0D0 /
!----------------------------------------------------------------------*
! wells

      IF (NVSWL.GT.0) THEN
         CALL FINPUT (WLD, TIH, UZNOW, UZNEXT, WLLAST, WLTIME, RWELIN, &
            NVSWL, WLNOW)

         IF (EQMARKER(WLTIME)) CALL ERROR(FFFATAL, 1042, PPPRI, 0, 0, &
            'End of well abstraction file (WLD)')



      ENDIF
! lateral flow boundary condition

      IF (NVSLF.GT.0) THEN
         CALL FINPUT (LFB, TIH, UZNOW, UZNEXT, RLFLST, RLFTIM, RLFPRV, &
            NVSLFT, RLFDUM)

         IF (EQMARKER(RLFTIM)) CALL ERROR(FFFATAL, 1043, PPPRI, 0, 0, &
            'End of lateral flow boundary condition file (LFB)')
         III = 1
         DO 20 I = 1, NVSLF
            NDUM = NVSLFN (I)
            IF (NDUM.EQ.0) NDUM = 1
            DO 10 II = 1, NDUM
               RLFNOW (II, I) = RLFDUM (III)
               III = III + 1
10          END DO

20       END DO



      ENDIF
! lateral head boundary condition

      IF (NVSLH.GT.0) THEN
         CALL HINPUT (LHB, TIH, UZNOW, UZNEXT, RLHLST, RLHTIM, RLHPRV, &
            RLHNXT, NVSLHT, RLHDUM)

         IF (EQMARKER(RLHTIM)) CALL ERROR(FFFATAL, 1044, PPPRI, 0, 0, &
            'End of lateral head boundary condition file (LHB)')
         III = 1
         DO 40 I = 1, NVSLH
            NDUM = NVSLHN (I)
            IF (NDUM.EQ.0) NDUM = 1
            DO 30 II = 1, NDUM
               RLHNOW (II, I) = RLHDUM (III)
               III = III + 1
30          END DO

40       END DO



      ENDIF
! lateral head gradient boundary condition

      IF (NVSLG.GT.0) THEN
         CALL HINPUT (LGB, TIH, UZNOW, UZNEXT, RLGLST, RLGTIM, RLGPRV, &
            RLGNXT, NVSLGT, RLGDUM)

         IF (EQMARKER(RLGTIM)) CALL ERROR(FFFATAL, 1052, PPPRI, 0, 0, &
            'End of lateral head gradient boundary condition file (LGB)')
         III = 1
         DO 60 I = 1, NVSLG
            NDUM = NVSLGN (I)
            IF (NDUM.EQ.0) NDUM = 1
            DO 50 II = 1, NDUM
               RLGNOW (II, I) = RLGDUM (III)
               III = III + 1
50          END DO

60       END DO



      ENDIF
! column base flow boundary condition

      IF (NVSBF.GT.0) THEN
         CALL FINPUT (BFB, TIH, UZNOW, UZNEXT, RBFLST, RBFTIM, RBFPRV, &
            NVSBF, RBFNOW)

         IF (EQMARKER(RBFTIM)) CALL ERROR(FFFATAL, 1045, PPPRI, 0, 0, &
            'End of column base flow boundary condition file (BFB)')



      ENDIF
! column base head boundary condition

      IF (NVSBH.GT.0) THEN
         CALL HINPUT (BHB, TIH, UZNOW, UZNEXT, RBHLST, RBHTIM, RBHPRV, &
            RBHNXT, NVSBH, RBHNOW)

         IF (EQMARKER(RBHTIM)) CALL ERROR(FFFATAL, 1046, PPPRI, 0, 0, &
            'End of column base head boundary condition file (BHB)')


      ENDIF
   END SUBROUTINE VSPREP


END MODULE
